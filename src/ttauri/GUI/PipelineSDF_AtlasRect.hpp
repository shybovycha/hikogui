// Copyright 2019 Pokitec
// All rights reserved.

#pragma once

#include "../required.hpp"
#include "../ivec.hpp"
#include "../vec.hpp"
#include "../aarect.hpp"
#include <array>

namespace tt::PipelineSDF {

/* A location inside the atlas where the character is located.
 */
struct AtlasRect {
    ivec atlasPosition;
    ivec atlasExtent;

    std::array<f32x4,4> textureCoords;

    AtlasRect(ivec atlasPosition, f32x4 drawExtent) noexcept;
};

}
