

#include "PipelineImage_DeviceShared.hpp"
#include "PipelineImage_Image.hpp"
#include "TTauri/Application.hpp"

#include <boost/numeric/conversion/cast.hpp>
#include <boost/range/combine.hpp>

namespace TTauri::GUI {

using namespace std;

PipelineImage::DeviceShared::DeviceShared(const std::shared_ptr<Device_vulkan> device) :
    device(move(device))
{
    buildIndexBuffer();
    buildShaders();
    buildAtlas();
}

PipelineImage::DeviceShared::~DeviceShared()
{
}

void PipelineImage::DeviceShared::destroy(gsl::not_null<Device_vulkan *> vulkanDevice)
{
    teardownIndexBuffer(vulkanDevice);
    teardownShaders(vulkanDevice);
    teardownAtlas(vulkanDevice);
}

std::vector<uint16_t> PipelineImage::DeviceShared::getFreeSlices(size_t const nrSlices)
{
    while (nrSlices > atlasFreeSlices.size()) {
        addAtlasImage();
    }

    auto slices = std::vector<uint16_t>();
    for (size_t i = 0; i < nrSlices; i++) {
        auto const slice = atlasFreeSlices.back();
        slices.push_back(slice);
        atlasFreeSlices.pop_back();
    }
    return slices;
}

std::shared_ptr<PipelineImage::Image> PipelineImage::DeviceShared::retainImage(const std::string &key, u16vec2 const extent)
{
    auto const i = viewImages.find(key);
    if (i != viewImages.end()) {
        auto image = (*i).second;
        image->retainCount++;
        return image;
    }

    auto const sliceExtent = u16vec2{
        (extent.x + (atlasSliceWidth - 1)) / atlasSliceWidth,
        (extent.y + (atlasSliceHeight - 1)) / atlasSliceHeight
    };

    auto const slices = getFreeSlices(sliceExtent.x * sliceExtent.y);
    auto image = TTauri::make_shared<PipelineImage::Image>(key, extent, sliceExtent, slices);
    viewImages.insert_or_assign(key, image);
    return image;
}

void PipelineImage::DeviceShared::releaseImage(const std::shared_ptr<PipelineImage::Image> &image)
{
    if (--image->retainCount == 0) {
        auto const i = viewImages.find(image->key);
        if (i != viewImages.end()) {
            viewImages.erase(i);
        } else {
            BOOST_THROW_EXCEPTION(Error());
        }
    }
}

void PipelineImage::DeviceShared::exchangeImage(std::shared_ptr<PipelineImage::Image> &image, const std::string &key, const u16vec2 extent)
{
    if (image && image->key == key) {
        return;
    }

    if (image) {
        releaseImage(image);
    }
    
    image = retainImage(key, extent);
}

void PipelineImage::DeviceShared::drawInCommandBuffer(vk::CommandBuffer &commandBuffer)
{
    auto const vulkanDevice = device.lock();

    commandBuffer.bindIndexBuffer(indexBuffer, 0, vk::IndexType::eUint16);
}


void PipelineImage::DeviceShared::buildIndexBuffer()
{
    auto const vulkanDevice = device.lock();

    // Create vertex index buffer
    {
        vk::BufferCreateInfo const bufferCreateInfo = {
            vk::BufferCreateFlags(),
            sizeof (uint16_t) * PipelineImage::maximumNumberOfIndices,
            vk::BufferUsageFlagBits::eIndexBuffer | vk::BufferUsageFlagBits::eTransferDst,
            vk::SharingMode::eExclusive
        };
        VmaAllocationCreateInfo allocationCreateInfo = {};
        allocationCreateInfo.usage = VMA_MEMORY_USAGE_GPU_ONLY;
        tie(indexBuffer, indexBufferAllocation) = vulkanDevice->createBuffer(bufferCreateInfo, allocationCreateInfo);
    }

    // Fill in the vertex index buffer, using a staging buffer, then copying.
    {
        // Create staging vertex index buffer.
        vk::BufferCreateInfo const bufferCreateInfo = {
            vk::BufferCreateFlags(),
            sizeof (uint16_t) * PipelineImage::maximumNumberOfIndices,
            vk::BufferUsageFlagBits::eIndexBuffer | vk::BufferUsageFlagBits::eTransferSrc,
            vk::SharingMode::eExclusive
        };
        VmaAllocationCreateInfo allocationCreateInfo = {};
        allocationCreateInfo.usage = VMA_MEMORY_USAGE_CPU_ONLY;
        auto const [stagingVertexIndexBuffer, stagingVertexIndexBufferAllocation] = vulkanDevice->createBuffer(bufferCreateInfo, allocationCreateInfo);

        // Initialize indices.
        auto const stagingVertexIndexBufferData = vulkanDevice->mapMemory<uint16_t>(stagingVertexIndexBufferAllocation);
        for (size_t i = 0; i < PipelineImage::maximumNumberOfIndices; i++) {
            auto const vertexInRectangle = i % 6;
            auto const rectangleNr = i / 6;
            auto const rectangleBase = rectangleNr * 4;

            switch (vertexInRectangle) {
            case 0: gsl::at(stagingVertexIndexBufferData, i) = rectangleBase + 0; break;
            case 1: gsl::at(stagingVertexIndexBufferData, i) = rectangleBase + 1; break;
            case 2: gsl::at(stagingVertexIndexBufferData, i) = rectangleBase + 2; break;
            case 3: gsl::at(stagingVertexIndexBufferData, i) = rectangleBase + 2; break;
            case 4: gsl::at(stagingVertexIndexBufferData, i) = rectangleBase + 1; break;
            case 5: gsl::at(stagingVertexIndexBufferData, i) = rectangleBase + 3; break;
            }
        }
        vmaFlushAllocation(vulkanDevice->allocator, stagingVertexIndexBufferAllocation, 0, VK_WHOLE_SIZE);
        vulkanDevice->unmapMemory(stagingVertexIndexBufferAllocation);

        // Copy indices to vertex index buffer.
        auto commands = vulkanDevice->intrinsic.allocateCommandBuffers({
            vulkanDevice->graphicsCommandPool, 
            vk::CommandBufferLevel::ePrimary, 
            1
            }).at(0);
        commands.begin({vk::CommandBufferUsageFlagBits::eOneTimeSubmit});
        commands.copyBuffer(stagingVertexIndexBuffer, indexBuffer, {{0, 0, sizeof (uint16_t) * PipelineImage::maximumNumberOfIndices}});
        commands.end();

        vector<vk::CommandBuffer> const commandBuffersToSubmit = { commands };
        vector<vk::SubmitInfo> const submitInfo = { { 0, nullptr, nullptr, boost::numeric_cast<uint32_t>(commandBuffersToSubmit.size()), commandBuffersToSubmit.data(), 0, nullptr } };
        vulkanDevice->graphicsQueue.submit(submitInfo, vk::Fence());
        vulkanDevice->graphicsQueue.waitIdle();

        vulkanDevice->intrinsic.freeCommandBuffers(vulkanDevice->graphicsCommandPool, {commands});
        vulkanDevice->destroyBuffer(stagingVertexIndexBuffer, stagingVertexIndexBufferAllocation);
    }
}

void PipelineImage::DeviceShared::teardownIndexBuffer(gsl::not_null<Device_vulkan *> vulkanDevice)
{
    vulkanDevice->destroyBuffer(indexBuffer, indexBufferAllocation);
}

void PipelineImage::DeviceShared::buildShaders()
{
    auto vulkanDevice = device.lock();

    vertexShaderModule = vulkanDevice->loadShader(get_singleton<Application>()->resourceDir / "PipelineImage.vert.spv");
    fragmentShaderModule = vulkanDevice->loadShader(get_singleton<Application>()->resourceDir / "PipelineImage.frag.spv");

    shaderStages = {
        {vk::PipelineShaderStageCreateFlags(), vk::ShaderStageFlagBits::eVertex, vertexShaderModule, "main"},
        {vk::PipelineShaderStageCreateFlags(), vk::ShaderStageFlagBits::eFragment, fragmentShaderModule, "main"}
    };
}

void PipelineImage::DeviceShared::teardownShaders(gsl::not_null<Device_vulkan *> vulkanDevice)
{
    vulkanDevice->intrinsic.destroy(vertexShaderModule);
    vulkanDevice->intrinsic.destroy(fragmentShaderModule);
}

void PipelineImage::DeviceShared::addAtlasImage()
{
    auto vulkanDevice = device.lock();
    auto const currentImageIndex = atlasImages.size();

    // Create atlas image
    vk::ImageCreateInfo const imageCreateInfo = {
        vk::ImageCreateFlags(),
        vk::ImageType::e2D,
        vk::Format::eR8G8B8A8Unorm,
        vk::Extent3D(4096, 4096, 1),
        1, // mipLevels
        1, // arrayLayers
        vk::SampleCountFlagBits::e1,
        vk::ImageTiling::eOptimal,
        vk::ImageUsageFlagBits::eTransferDst | vk::ImageUsageFlagBits::eSampled,
        vk::SharingMode::eExclusive,
        0, nullptr,
        vk::ImageLayout::eUndefined
    };
    VmaAllocationCreateInfo allocationCreateInfo = {};
    allocationCreateInfo.usage = VMA_MEMORY_USAGE_GPU_ONLY;

    auto const [atlasImage, atlasImageAllocation] = vulkanDevice->createImage(imageCreateInfo, allocationCreateInfo);

    auto const atlasImageView = vulkanDevice->intrinsic.createImageView({
        vk::ImageViewCreateFlags(),
        atlasImage,
        vk::ImageViewType::e2D,
        imageCreateInfo.format,
        vk::ComponentMapping(),
        {
            vk::ImageAspectFlagBits::eColor,
            0, // baseMipLevel
            1, // levelCount
            0, // baseArrayLayer
            1 // layerCount
        }
    });

    atlasImages.push_back(atlasImage);
    atlasImageAllocations.push_back(atlasImageAllocation);
    atlasImageViews.push_back(atlasImageView);

    // Add slices for this image to free list.
    size_t const sliceOffset = currentImageIndex * atlasNrSlicesPerImage;
    for (size_t i = 0; i < atlasNrSlicesPerImage; i++) {
        atlasFreeSlices.push_back(sliceOffset + i);
    }

    // Build image descriptor info.
    for (size_t i = 0; i < atlasDescriptorImageInfos.size(); i++) {
        // Point the descriptors to each imageView,
        // repeat the first imageView if there are not enough.
        atlasDescriptorImageInfos.at(i) = {
            vk::Sampler(),
            i < atlasImageViews.size() ? atlasImageViews.at(i) : atlasImageViews.at(0),
            vk::ImageLayout::eShaderReadOnlyOptimal
        };
    }
}

void PipelineImage::DeviceShared::buildAtlas()
{
    auto vulkanDevice = device.lock();

    // Create staging image
    vk::ImageCreateInfo const imageCreateInfo = {
        vk::ImageCreateFlags(),
        vk::ImageType::e2D,
        vk::Format::eR8G8B8A8Unorm,
        vk::Extent3D(4096, 4096, 1),
        1, // mipLevels
        1, // arrayLayers
        vk::SampleCountFlagBits::e1,
        vk::ImageTiling::eLinear,
        vk::ImageUsageFlagBits::eTransferSrc,
        vk::SharingMode::eExclusive,
        0, nullptr,
        vk::ImageLayout::eUndefined
    };
    VmaAllocationCreateInfo allocationCreateInfo = {};
    allocationCreateInfo.usage = VMA_MEMORY_USAGE_CPU_TO_GPU;
    tie(stagingImage, stagingImageAllocation) = vulkanDevice->createImage(imageCreateInfo, allocationCreateInfo);

    vk::SamplerCreateInfo const samplerCreateInfo = {
        vk::SamplerCreateFlags(),
        vk::Filter::eNearest, // magFilter
        vk::Filter::eNearest, // minFilter
        vk::SamplerMipmapMode::eNearest, // mipmapMode
        vk::SamplerAddressMode::eRepeat, // addressModeU
        vk::SamplerAddressMode::eRepeat, // addressModeV
        vk::SamplerAddressMode::eRepeat, // addressModeW
        0.0, // mipLodBias
        FALSE, // anisotropyEnable
        0.0, // maxAnisotropy
        FALSE, // compareEnable
        vk::CompareOp::eNever,
        0.0, // minLod
        0.0, // maxLod
        vk::BorderColor::eFloatTransparentBlack,
        FALSE // unnormazlizedCoordinates
    };
    atlasSampler = vulkanDevice->intrinsic.createSampler(samplerCreateInfo);

    atlasSamplerDescriptorImageInfo = {
        atlasSampler,
        vk::ImageView(),
        vk::ImageLayout::eUndefined
    };

    // There needs to be at least one atlas image, so the array of samplers can point to
    // the single image.
    addAtlasImage();
}

void PipelineImage::DeviceShared::teardownAtlas(gsl::not_null<Device_vulkan *> vulkanDevice)
{
    vulkanDevice->intrinsic.destroy(atlasSampler);

    for (size_t i = 0; i < atlasImages.size(); i++) {
        vulkanDevice->intrinsic.destroy(atlasImageViews.at(i));
        vulkanDevice->destroyImage(atlasImages.at(i), atlasImageAllocations.at(i));
    }
    atlasImages.clear();
    atlasImageAllocations.clear();
    atlasImageViews.clear();

    vulkanDevice->destroyImage(stagingImage, stagingImageAllocation);
}

}