// This file was generated by generate_unicode_data.py

#pragma once

#include "../utility/module.hpp"
#include <cstdint>
#include <optional>

namespace hi {
inline namespace v1 {
namespace detail {

constexpr auto ucd_bidi_paired_bracket_types_chunk_size = 128_uz;
constexpr auto ucd_bidi_paired_bracket_types_index_width = 4_uz;
constexpr auto ucd_bidi_paired_bracket_types_indices_size = 512_uz;
constexpr auto ucd_bidi_paired_bracket_type_width = 2_uz;

static_assert(std::has_single_bit(ucd_bidi_paired_bracket_types_chunk_size));

constexpr uint8_t ucd_bidi_paired_bracket_types_indices_bytes[272] = {
     1, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 33, 17, 17, 17, 17, 17, 17, 19, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    69, 17, 17, 97, 17, 17, 17,120, 17, 25, 17, 17, 17, 17,161, 17,177, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,193,209,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

constexpr uint8_t ucd_bidi_paired_bracket_types_bytes[464] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 96,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1, 32,  0,  0,  0,  0,  0,  0,  1, 32,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  6, 96,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  1,128,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 24,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 24,
     0,  0,  0, 24,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,102,  0,  0,  0,  0,  0,  0,  0, 24,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,102,102,102, 96,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 24,  0,  0,  0,  0,  0,  0,  0,  6,102,102,  0,  0,  0,  0,
     1,153,153,153,153,153,128,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,102,  0,  0,  0,  0,  0,  0,  0,  0, 96,
     0,  0,  0,  0,  0,  0,  0,  0,  6,102, 96,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 25,153,128,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,102,102, 96,102,102,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 25,152,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0, 96,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1, 32,  0,  0,  0,  0,  0,  0,  1, 33,134,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};


} // namespace detail

enum class unicode_bidi_paired_bracket_type : uint8_t {
    n = 0,
    o = 1,
    c = 2,
};

[[nodiscard]] constexpr unicode_bidi_paired_bracket_type ucd_get_bidi_paired_bracket_type(char32_t code_point) noexcept
{
    constexpr auto max_code_point_hi = detail::ucd_bidi_paired_bracket_types_indices_size - 1;

    auto code_point_hi = code_point / detail::ucd_bidi_paired_bracket_types_chunk_size;
    hilet code_point_lo = code_point % detail::ucd_bidi_paired_bracket_types_chunk_size;

    if (code_point_hi > max_code_point_hi) {
        code_point_hi = max_code_point_hi;
    }

    hilet chunk_index = load_bits_be<detail::ucd_bidi_paired_bracket_types_index_width>(
        detail::ucd_bidi_paired_bracket_types_indices_bytes,
        code_point_hi * detail::ucd_bidi_paired_bracket_types_index_width);

    // Add back in the lower-bits of the code-point.
    hilet index = (chunk_index * detail::ucd_bidi_paired_bracket_types_chunk_size) + code_point_lo;

    // Get the canonical combining class from the table.
    hilet value = load_bits_be<detail::ucd_bidi_paired_bracket_type_width>(
        detail::ucd_bidi_paired_bracket_types_bytes, index * detail::ucd_bidi_paired_bracket_type_width);

    return static_cast<unicode_bidi_paired_bracket_type>(value);
}

}} // namespace hi::v1

