// Copyright Take Vos 2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "../geometry/module.hpp"
#include "../utility/module.hpp"
#include <cstdint>
#include <limits>
#include <concepts>

namespace hi { inline namespace v1 {

/** 2D constraints.
 * @ingroup geometry
 *
 * This type holds multiple possible sizes that an 2D object may be.
 * We need multiple sizes in case there is a non-linear relation between the width and height of an object.
 *
 */
struct box_constraints {
    extent2 minimum = {};
    extent2 preferred = {};
    extent2 maximum = {};
    hi::margins margins = {};
    float priority = 0.0f;
    hi::alignment alignment = hi::alignment{};

    constexpr box_constraints() noexcept = default;
    constexpr box_constraints(box_constraints const&) noexcept = default;
    constexpr box_constraints(box_constraints&&) noexcept = default;
    constexpr box_constraints& operator=(box_constraints const&) noexcept = default;
    constexpr box_constraints& operator=(box_constraints&&) noexcept = default;
    [[nodiscard]] constexpr friend bool operator==(box_constraints const&, box_constraints const&) noexcept = default;

    constexpr box_constraints(
        extent2 minimum,
        extent2 preferred,
        extent2 maximum,
        hi::alignment alignment = hi::alignment{},
        hi::margins margins = hi::margins{},
        float priority = 0.0f) noexcept :
        minimum(minimum), preferred(preferred), maximum(maximum), margins(margins), priority(priority), alignment(alignment)
    {
        hi_axiom(holds_invariant());
    }

    [[nodiscard]] constexpr box_constraints constrain(extent2 new_minimum, extent2 new_maximum) const noexcept
    {
        hi_assert(new_minimum <= new_maximum);

        auto r = *this;

        inplace_max(r.minimum, new_minimum);
        inplace_min(r.maximum, new_maximum);

        inplace_max(r.preferred, r.minimum);
        inplace_max(r.maximum, r.preferred);
        hi_axiom(r.holds_invariant());
        return r;
    }

    constexpr box_constraints& operator+=(extent2 const& rhs) noexcept
    {
        minimum.width() += rhs.width();
        preferred.width() += rhs.width();
        maximum.width() += rhs.width();
        minimum.height() += rhs.height();
        preferred.height() += rhs.height();
        maximum.height() += rhs.height();

        hi_axiom(holds_invariant());
        return *this;
    }

    [[nodiscard]] constexpr box_constraints operator+(extent2 const& rhs) const noexcept
    {
        auto r = *this;
        r += rhs;
        return r;
    }

    [[nodiscard]] constexpr bool holds_invariant() const noexcept
    {
        if (alignment == horizontal_alignment::flush or alignment == horizontal_alignment::justified) {
            return false;
        }
        if (not(is_integral_value(minimum.width()) and is_integral_value(minimum.height()) and
                is_integral_value(preferred.width()) and is_integral_value(preferred.height()) and
                is_integral_value(maximum.width()) and is_integral_value(maximum.height()))) {
            return false;
        }
        if (minimum > preferred or preferred > maximum) {
            return false;
        }
        return true;
    }

    [[nodiscard]] friend constexpr box_constraints max(box_constraints const& lhs, extent2 const& rhs) noexcept
    {
        auto r = lhs;
        inplace_max(r.minimum, rhs);
        inplace_max(r.preferred, rhs);
        inplace_max(r.maximum, rhs);

        hi_axiom(r.holds_invariant());
        return r;
    }

    /** Makes a constraint that encompasses both given constraints.
     *
     * @note The alignment is selected from the left-hand-side.
     * @param lhs The left hand side box constraints.
     * @param rhs The right hand side box constraints.
     * @return A box constraints that encompasses both given constraints.
     */
    [[nodiscard]] friend constexpr box_constraints max(box_constraints const& lhs, box_constraints const& rhs) noexcept
    {
        auto r = lhs;
        inplace_max(r.minimum, rhs.minimum);
        inplace_max(r.preferred, rhs.preferred);
        inplace_max(r.maximum, rhs.maximum);
        inplace_max(r.margins, rhs.margins);
        inplace_max(r.priority, rhs.priority);

        hi_axiom(r.holds_invariant());
        return r;
    }

    template<std::convertible_to<box_constraints>... Args>
    [[nodiscard]] friend constexpr box_constraints
    max(box_constraints const& first, box_constraints const& second, box_constraints const& third, Args const&...args) noexcept
    {
        return max(first, max(second, third, args...));
    }
};

}} // namespace hi::v1
