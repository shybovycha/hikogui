// Copyright Take Vos 2020-2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "../utility/utility.hpp"
#include "../geometry/module.hpp"
#include "../macros.hpp"
#include <vma/vk_mem_alloc.h>
#include <vulkan/vulkan.hpp>
#include <mutex>



namespace hi::inline v1 {
class gfx_device;

namespace pipeline_tone_mapper {
struct Image;

struct device_shared final {
    gfx_device const &device;

    vk::ShaderModule vertexShaderModule;
    vk::ShaderModule fragmentShaderModule;
    std::vector<vk::PipelineShaderStageCreateInfo> shaderStages;

    device_shared(gfx_device const &device);
    ~device_shared();

    device_shared(device_shared const &) = delete;
    device_shared &operator=(device_shared const &) = delete;
    device_shared(device_shared &&) = delete;
    device_shared &operator=(device_shared &&) = delete;

    /*! Deallocate vulkan resources.
     * This is called in the destructor of gfx_device, therefor we can not use our `std::weak_ptr<gfx_device>
     * device`.
     */
    void destroy(gfx_device const *vulkanDevice);

    void drawInCommandBuffer(vk::CommandBuffer const &commandBuffer);

private:
    void buildShaders();
    void teardownShaders(gfx_device const*vulkanDevice);
};

} // namespace pipeline_tone_mapper
} // namespace hi::inline v1
