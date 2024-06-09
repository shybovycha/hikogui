// Copyright Take Vos 2024.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "style_computed_properties.hpp"
#include "style_priority.hpp"
#include "../text/text.hpp"
#include "../units/units.hpp"
#include "../color/color.hpp"
#include "../geometry/geometry.hpp"
#include "../macros.hpp"
#include <cstdint>

hi_export_module(hikogui.theme : style_properties);

hi_export namespace hi {
inline namespace v1 {

class style_properties {
public:
    constexpr style_properties() noexcept = default;
    constexpr style_properties(style_properties const&) noexcept = default;
    constexpr style_properties(style_properties&&) noexcept = default;
    constexpr style_properties& operator=(style_properties const&) noexcept = default;
    constexpr style_properties& operator=(style_properties&&) noexcept = default;

    [[nodiscard]] friend style_computed_properties operator*(style_properties const& lhs, unit::pixel_density const& rhs) noexcept
    {
        auto r = style_computed_properties{};

#define HIX_MUL(NAME, UNIT, ROUND) \
    r.NAME = ROUND(UNIT, lhs._##NAME * rhs); \
    r._##NAME##_inherit = lhs._##NAME##_inherit;

        HIX_MUL(width, unit::pixels, ceil_as);
        HIX_MUL(height, unit::pixels, ceil_as);
        HIX_MUL(font_size, unit::pixels_per_em, round_as);
        HIX_MUL(margin_left, unit::pixels, round_as);
        HIX_MUL(margin_bottom, unit::pixels, round_as);
        HIX_MUL(margin_right, unit::pixels, round_as);
        HIX_MUL(margin_top, unit::pixels, round_as);
        HIX_MUL(padding_left, unit::pixels, round_as);
        HIX_MUL(padding_bottom, unit::pixels, round_as);
        HIX_MUL(padding_right, unit::pixels, round_as);
        HIX_MUL(padding_top, unit::pixels, round_as);
        HIX_MUL(border_bottom_left_radius, unit::pixels, round_as);
        HIX_MUL(border_bottom_right_radius, unit::pixels, round_as);
        HIX_MUL(border_top_left_radius, unit::pixels, round_as);
        HIX_MUL(border_top_right_radius, unit::pixels, round_as);
#undef HIX_MUL

        r.border_width = std::max(floor_as(unit::pixels, lhs._border_width * rhs), unit::pixels(1.0f));
        r._border_width_inherit = lhs._border_width_inherit;

#define HIX_COPY(NAME) \
    r.NAME = lhs._##NAME; \
    r._##NAME##_inherit = lhs._##NAME##_inherit;

        HIX_COPY(color);
        HIX_COPY(background_color);
        HIX_COPY(border_color);
        HIX_COPY(accent_color);
        HIX_COPY(horizontal_alignment);
        HIX_COPY(vertical_alignment);
        HIX_COPY(text_style);
#undef HIX_COPY

        return r;
    }

    [[nodiscard]] friend style_computed_properties operator*(unit::pixel_density const& lhs, style_properties const& rhs) noexcept
    {
        return rhs * lhs;
    }

#define HIX_GETSET(TYPE, NAME) \
    [[nodiscard]] TYPE NAME() const noexcept \
    { \
        return _##NAME; \
    } \
    void set_##NAME(TYPE NAME, style_priority priority) noexcept \
    { \
        if (priority >= _##NAME##_priority) { \
            _##NAME##_priority = priority; \
            _##NAME##_inherit = 0; \
            _##NAME = NAME; \
        } \
    } \
    void inherit_##NAME(style_priority priority) noexcept \
    { \
        if (priority >= _##NAME##_priority) { \
            _##NAME##_priority = priority; \
            _##NAME##_inherit = 1; \
            _##NAME = {}; \
        } \
    } \
    void reset_##NAME() noexcept \
    { \
        _##NAME##_priority = style_priority{}; \
        _##NAME##_inherit = 1; \
        _##NAME = {}; \
    }

    HIX_GETSET(hi::unit::length_f, width)
    HIX_GETSET(hi::unit::length_f, height)
    HIX_GETSET(hi::unit::font_size_f, font_size)
    HIX_GETSET(hi::unit::length_f, margin_left)
    HIX_GETSET(hi::unit::length_f, margin_bottom)
    HIX_GETSET(hi::unit::length_f, margin_right)
    HIX_GETSET(hi::unit::length_f, margin_top)
    HIX_GETSET(hi::unit::length_f, padding_left)
    HIX_GETSET(hi::unit::length_f, padding_bottom)
    HIX_GETSET(hi::unit::length_f, padding_right)
    HIX_GETSET(hi::unit::length_f, padding_top)
    HIX_GETSET(hi::unit::length_f, border_width)
    HIX_GETSET(hi::unit::length_f, border_bottom_left_radius)
    HIX_GETSET(hi::unit::length_f, border_bottom_right_radius)
    HIX_GETSET(hi::unit::length_f, border_top_left_radius)
    HIX_GETSET(hi::unit::length_f, border_top_right_radius)
    HIX_GETSET(hi::color, color)
    HIX_GETSET(hi::color, background_color)
    HIX_GETSET(hi::color, border_color)
    HIX_GETSET(hi::color, accent_color)
    HIX_GETSET(hi::horizontal_alignment, horizontal_alignment)
    HIX_GETSET(hi::vertical_alignment, vertical_alignment)
    HIX_GETSET(hi::text_style_set, text_style)
#undef HIX_GETSET

    void set_margin(unit::length_f margin, style_priority priority) noexcept
    {
        set_margin_left(margin, priority);
        set_margin_bottom(margin, priority);
        set_margin_right(margin, priority);
        set_margin_top(margin, priority);
    }

    void set_padding(unit::length_f padding, style_priority priority) noexcept
    {
        set_padding_left(padding, priority);
        set_padding_bottom(padding, priority);
        set_padding_right(padding, priority);
        set_padding_top(padding, priority);
    }

    void set_border_radius(unit::length_f border_radius, style_priority priority) noexcept
    {
        set_border_bottom_left_radius(border_radius, priority);
        set_border_bottom_right_radius(border_radius, priority);
        set_border_top_left_radius(border_radius, priority);
        set_border_top_right_radius(border_radius, priority);
    }

    void reset() noexcept
    {
        reset_width();
        reset_height();
        reset_font_size();
        reset_margin_left();
        reset_margin_bottom();
        reset_margin_right();
        reset_margin_top();
        reset_padding_left();
        reset_padding_bottom();
        reset_padding_right();
        reset_padding_top();
        reset_border_width();
        reset_border_bottom_left_radius();
        reset_border_bottom_right_radius();
        reset_border_top_left_radius();
        reset_border_top_right_radius();
        reset_color();
        reset_background_color();
        reset_border_color();
        reset_accent_color();
        reset_horizontal_alignment();
        reset_vertical_alignment();
        reset_text_style();
    }

    /** Apply attributes of other ontop of the current.
     * 
     * @param other The attributes used to overwrite the current attributes.
     * @return A mask for what kind of values where changed.
     */
    void apply(style_properties const& other) noexcept
    {
#define HIX_APPLY(NAME) \
    if (other._##NAME##_priority >= _##NAME##_priority) { \
        _##NAME = other._##NAME; \
        _##NAME##_priority = other._##NAME##_priority; \
        _##NAME##_inherit = other._##NAME##_inherit; \
    }

        HIX_APPLY(width)
        HIX_APPLY(height)
        HIX_APPLY(font_size)
        HIX_APPLY(margin_left)
        HIX_APPLY(margin_bottom)
        HIX_APPLY(margin_right)
        HIX_APPLY(margin_top)
        HIX_APPLY(padding_left)
        HIX_APPLY(padding_bottom)
        HIX_APPLY(padding_right)
        HIX_APPLY(padding_top)
        HIX_APPLY(border_width)
        HIX_APPLY(border_bottom_left_radius)
        HIX_APPLY(border_bottom_right_radius)
        HIX_APPLY(border_top_left_radius)
        HIX_APPLY(border_top_right_radius)
        HIX_APPLY(color)
        HIX_APPLY(background_color)
        HIX_APPLY(border_color)
        HIX_APPLY(accent_color)
        HIX_APPLY(horizontal_alignment)
        HIX_APPLY(vertical_alignment)
        HIX_APPLY(text_style)
#undef HIX_APPLY
    }

private:
    hi::unit::length_f _width = unit::points(0.0f);
    hi::unit::length_f _height = unit::points(0.0f);
    hi::unit::font_size_f _font_size = unit::points_per_em(0.0f);
    hi::unit::length_f _margin_left = unit::points(0.0f);
    hi::unit::length_f _margin_bottom = unit::points(0.0f);
    hi::unit::length_f _margin_right = unit::points(0.0f);
    hi::unit::length_f _margin_top = unit::points(0.0f);
    hi::unit::length_f _padding_left = unit::points(0.0f);
    hi::unit::length_f _padding_bottom = unit::points(0.0f);
    hi::unit::length_f _padding_right = unit::points(0.0f);
    hi::unit::length_f _padding_top = unit::points(0.0f);
    hi::unit::length_f _border_width = unit::points(0.0f);
    hi::unit::length_f _border_bottom_left_radius = unit::points(0.0f);
    hi::unit::length_f _border_bottom_right_radius = unit::points(0.0f);
    hi::unit::length_f _border_top_left_radius = unit::points(0.0f);
    hi::unit::length_f _border_top_right_radius = unit::points(0.0f);
    hi::color _color = {};
    hi::color _background_color = {};
    hi::color _border_color = {};
    hi::color _accent_color = {};
    hi::horizontal_alignment _horizontal_alignment = hi::horizontal_alignment::left;
    hi::vertical_alignment _vertical_alignment = hi::vertical_alignment::top;
    hi::text_style_set _text_style = {};

    size_t _width_inherit : 1 = 1;
    size_t _height_inherit : 1 = 1;
    size_t _font_size_inherit : 1 = 1;
    size_t _margin_left_inherit : 1 = 1;
    size_t _margin_bottom_inherit : 1 = 1;
    size_t _margin_right_inherit : 1 = 1;
    size_t _margin_top_inherit : 1 = 1;
    size_t _padding_left_inherit : 1 = 1;
    size_t _padding_bottom_inherit : 1 = 1;
    size_t _padding_right_inherit : 1 = 1;
    size_t _padding_top_inherit : 1 = 1;
    size_t _border_width_inherit : 1 = 1;
    size_t _border_bottom_left_radius_inherit : 1 = 1;
    size_t _border_bottom_right_radius_inherit : 1 = 1;
    size_t _border_top_left_radius_inherit : 1 = 1;
    size_t _border_top_right_radius_inherit : 1 = 1;
    size_t _color_inherit : 1 = 1;
    size_t _background_color_inherit : 1 = 1;
    size_t _border_color_inherit : 1 = 1;
    size_t _accent_color_inherit : 1 = 1;
    size_t _horizontal_alignment_inherit : 1 = 1;
    size_t _vertical_alignment_inherit : 1 = 1;
    size_t _text_style_inherit : 1 = 1;

    style_priority _width_priority;
    style_priority _height_priority;
    style_priority _font_size_priority;
    style_priority _margin_left_priority;
    style_priority _margin_bottom_priority;
    style_priority _margin_right_priority;
    style_priority _margin_top_priority;
    style_priority _padding_left_priority;
    style_priority _padding_bottom_priority;
    style_priority _padding_right_priority;
    style_priority _padding_top_priority;
    style_priority _border_width_priority;
    style_priority _border_bottom_left_radius_priority;
    style_priority _border_bottom_right_radius_priority;
    style_priority _border_top_left_radius_priority;
    style_priority _border_top_right_radius_priority;
    style_priority _color_priority;
    style_priority _background_color_priority;
    style_priority _border_color_priority;
    style_priority _accent_color_priority;
    style_priority _horizontal_alignment_priority;
    style_priority _vertical_alignment_priority;
    style_priority _text_style_priority;
};

} // namespace v1
}
