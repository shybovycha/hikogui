//
//  BackingPipeline.hpp
//  TTauri
//
//  Created by Tjienta Vara on 2019-02-12.
//  Copyright © 2019 Pokitec. All rights reserved.
//

#pragma once

#include "Pipeline_vulkan.hpp"
#include "config.hpp"
#include "geometry.hpp"

#include <vma/vk_mem_alloc.h>
#include <gsl/gsl>

namespace TTauri {
namespace GUI {

/*! Pipeline for rendering backings of widgets.
 * Maintains texture map atlasses and sharing for all views.
 */
class BackingPipeline_vulkan : public Pipeline_vulkan {
public:
    struct PushConstants {
        glm::vec2 windowExtent = { 0.0, 0.0 };
        glm::vec2 viewportScale = { 0.0, 0.0 };

        static std::vector<vk::PushConstantRange> pushConstantRanges()
        {
            return {
                { vk::ShaderStageFlagBits::eVertex | vk::ShaderStageFlagBits::eFragment, 0, sizeof(PushConstants) }
            };
        }
    };

    /*! A vertex defining a rectangle on a window.
     * The same vertex is passed to the vertex shader 6 times for each rectangle (two triangles).
     * The vertex shader will convert window pixel-coordinates to normalized projection-coordinates.
     */
    struct Vertex {
        //! The pixel-coordinates where the origin is located relative to the top-left corner of the window.
        glm::vec2 position;

        //! The left-top and right-bottom position in pixels of the clipping rectangle relative to the top-left corner of the window.
        u16vec4 clippingRectangle;

        //! The x, y coord inside the texture-atlas, z is used as an index in the texture-atlas array
        u16vec3 atlasPosition;

        //! The depth for depth test.
        uint16_t depth;

        //! transparency of the image.
        uint8_t alpha;

        //! Align to 32 bits.
        uint8_t dummy[3];
     

        static vk::VertexInputBindingDescription inputBindingDescription()
        {
            return {
                0, sizeof(Vertex), vk::VertexInputRate::eVertex
            };
        }

        static std::vector<vk::VertexInputAttributeDescription> inputAttributeDescriptions()
        {
            return {
                { 0, 0, vk::Format::eR32G32Sfloat, offsetof(Vertex, position) },
                { 1, 0, vk::Format::eR16G16B16A16Uint, offsetof(Vertex, clippingRectangle) },
                { 2, 0, vk::Format::eR16G16B16Uint, offsetof(Vertex, atlasPosition) },                
                { 3, 0, vk::Format::eR16Uint, offsetof(Vertex, depth) },
                { 4, 0, vk::Format::eR8Uint, offsetof(Vertex, alpha) },
            };
        }
    };

    class Delegate {
    public:
        struct Error : virtual boost::exception, virtual std::exception {};

        virtual size_t backingPipelineRender(const gsl::span<Vertex> &vertices, size_t offset) = 0;
    };

    BackingPipeline_vulkan(const std::shared_ptr<Window> &window);
    ~BackingPipeline_vulkan() {};

    BackingPipeline_vulkan(const BackingPipeline_vulkan &) = delete;
    BackingPipeline_vulkan &operator=(const BackingPipeline_vulkan &) = delete;
    BackingPipeline_vulkan(BackingPipeline_vulkan &&) = delete;
    BackingPipeline_vulkan &operator=(BackingPipeline_vulkan &&) = delete;

    vk::Semaphore render(uint32_t imageIndex, vk::Semaphore inputSemaphore) override;

protected:
    PushConstants pushConstants;

    size_t numberOfVertices = 0;
    std::vector<vk::Buffer> vertexBuffers;
    std::vector<VmaAllocation> vertexBuffersAllocation;
    std::vector<gsl::span<Vertex>> vertexBuffersData;

    vk::Buffer vertexIndexBuffer;
    VmaAllocation vertexIndexBufferAllocation;

    void drawInCommandBuffer(vk::CommandBuffer &commandBuffer, uint32_t imageIndex) override;
    std::vector<vk::ShaderModule> createShaderModules() const override;
    std::vector<vk::PipelineShaderStageCreateInfo> createShaderStages(const std::vector<vk::ShaderModule> &shaders) const override;
    std::vector<vk::PushConstantRange> createPushConstantRanges() const override;
    vk::VertexInputBindingDescription createVertexInputBindingDescription() const override;
    std::vector<vk::VertexInputAttributeDescription> createVertexInputAttributeDescriptions() const override;

    size_t maximumNumberOfVertices() const { return 65536; }
    size_t maximumNumberOfVertexIndices() const { return 6 * maximumNumberOfVertices(); }

    void buildVertexBuffers(size_t nrFrameBuffers) override;
    void teardownVertexBuffers() override;


};

}}
