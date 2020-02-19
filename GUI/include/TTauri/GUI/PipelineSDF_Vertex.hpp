// Copyright 2019 Pokitec
// All rights reserved.

#pragma once

#include "TTauri/Foundation/geometry.hpp"
#include "TTauri/Foundation/R16G16B16A16SFloat.hpp"
#include <vulkan/vulkan.hpp>

namespace TTauri::GUI::PipelineSDF {

/*! A vertex defining a rectangle on a window.
* The vertex shader will convert window pixel-coordinates to normalized projection-coordinates.
*/
struct Vertex {
    //! The pixel-coordinates where the origin is located relative to the bottom-left corner of the window.
    glm::vec3 position;

    //! Clipping rectangle. (x,y)=bottom-left, (z,w)=top-right
    glm::vec4 clippingRectangle;

    //! The x, y (relative to bottom-left) coordinate inside the texture-atlas, z is used as an index in the texture-atlas array
    glm::vec3 textureCoord;

    //! The color of the glyph.
    R16G16B16A16SFloat color;

    //! The multiplier to use to convert a SDF distance from texture space to screen-space.
    float distanceMultiplier;

    //! The number of pixels that the shadow is, negative values make inset shadow. Should be less than or equal to the SDF8::max_distance.
    float shadowSize;

    Vertex(glm::vec3 position, glm::vec4 clippingRectangle, glm::vec3 textureCoord, R16G16B16A16SFloat color, float distanceMultiplier, float shadowSize) noexcept :
        position(position),
        clippingRectangle(clippingRectangle),
        textureCoord(textureCoord),
        color(color),
        distanceMultiplier(distanceMultiplier),
        shadowSize(shadowSize) {}

    static vk::VertexInputBindingDescription inputBindingDescription()
    {
        return {
            0, sizeof(Vertex), vk::VertexInputRate::eVertex
        };
    }

    static std::vector<vk::VertexInputAttributeDescription> inputAttributeDescriptions()
    {
        return {
            { 0, 0, vk::Format::eR32G32B32Sfloat, offsetof(Vertex, position) },
            { 1, 0, vk::Format::eR32G32B32A32Sfloat, offsetof(Vertex, clippingRectangle) },
            { 2, 0, vk::Format::eR32G32B32Sfloat, offsetof(Vertex, textureCoord) },                
            { 3, 0, vk::Format::eR16G16B16A16Sfloat, offsetof(Vertex, color) },
            { 4, 0, vk::Format::eR32Sfloat, offsetof(Vertex, distanceMultiplier) },
            { 5, 0, vk::Format::eR32Sfloat, offsetof(Vertex, shadowSize) }
        };
    }
};
}
