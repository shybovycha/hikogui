// Copyright Take Vos 2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "box_constraints.hpp"
#include "../geometry/module.hpp"
#include "../utility/module.hpp"
#include <limits>
#include <optional>

namespace hi { inline namespace v1 {

struct box_shape {
    aarectangle rectangle;
    std::optional<float> baseline = {};
    std::optional<float> centerline = {};

    constexpr box_shape() noexcept = default;
    constexpr box_shape(box_shape const&) noexcept = default;
    constexpr box_shape(box_shape&&) noexcept = default;
    constexpr box_shape& operator=(box_shape const&) noexcept = default;
    constexpr box_shape& operator=(box_shape&&) noexcept = default;
    [[nodiscard]] constexpr friend bool operator==(box_shape const&, box_shape const&) noexcept = default;

    constexpr box_shape(extent2 size) noexcept : rectangle(size), baseline(), centerline()
    {
        hi_axiom(holds_invariant());
    }

    constexpr box_shape(
        override_t,
        box_constraints const& constraints,
        aarectangle const& rectangle,
        float baseline_adjustment) noexcept :
        rectangle(rectangle),
        baseline(make_guideline(
            constraints.alignment.vertical(),
            rectangle.bottom(),
            rectangle.top(),
            baseline_adjustment)),
        centerline(make_guideline(
            constraints.alignment.horizontal(),
            rectangle.left(),
            rectangle.right()))
    {
        hi_axiom(holds_invariant());
    }

    constexpr box_shape(box_constraints const& constraints, aarectangle rectangle, float baseline_adjustment) noexcept :
        box_shape(override_t{}, constraints, rectangle, baseline_adjustment)
    {
        hi_axiom(rectangle.size() >= constraints.minimum);
        hi_axiom(holds_invariant());
    }

    [[nodiscard]] constexpr float x() const noexcept
    {
        return rectangle.x();
    }

    [[nodiscard]] constexpr float y() const noexcept
    {
        return rectangle.y();
    }

    [[nodiscard]] constexpr extent2 size() const noexcept
    {
        return rectangle.size();
    }

    [[nodiscard]] constexpr float width() const noexcept
    {
        return rectangle.width();
    }

    [[nodiscard]] constexpr float height() const noexcept
    {
        return rectangle.height();
    }

    [[nodiscard]] constexpr bool holds_invariant() const noexcept
    {
        if (not(is_integral_value(rectangle.x()) and is_integral_value(rectangle.y()) and is_integral_value(rectangle.width()) and
                is_integral_value(rectangle.height()))) {
            return false;
        }
        if (baseline and not is_integral_value(*baseline)) {
            return false;
        }
        if (centerline and not is_integral_value(*centerline)) {
            return false;
        }
        return true;
    }
};

}} // namespace hi::v1
