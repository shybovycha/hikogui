// Copyright Take Vos 2023.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "otype_utilities.hpp"
#include "font_char_map.hpp"
#include "../macros.hpp"

hi_export_module(hikogui.font.otype_cmap);

hi_export namespace hi { inline namespace v1 {

[[nodiscard]] inline glyph_id otype_cmap_parse_map_4(std::span<std::byte const> over_sized_bytes, char32_t code_point)
{
    struct header_type {
        big_uint16_buf_t format;
        big_uint16_buf_t length;
        big_uint16_buf_t language;
        big_uint16_buf_t seg_count_x2;
        big_uint16_buf_t search_range;
        big_uint16_buf_t entry_selector;
        big_uint16_buf_t range_shift;
    };

    if (code_point > 0xffff) {
        return glyph_id{};
    }

    auto offset = 0_uz;
    auto const& header = implicit_cast<header_type>(offset, over_sized_bytes);
    hi_axiom(*header.format == 4);
    auto const length = *header.length;
    auto const bytes = hi_check_subspan(over_sized_bytes, 0, length);

    auto const seg_count = *header.seg_count_x2 / 2;

    auto const end_codes = implicit_cast<big_uint16_buf_t>(offset, bytes, seg_count);
    offset += sizeof(uint16_t); // reservedPad

    auto const end_code_it = std::lower_bounds(end_code.begin(), end_code.end(), code_point, [](auto& item, auto& value) {
        return *item < value;
    });
    if (end_code_it == end_code.end()) {
        return glyph_id{};
    }
    auto const segment_index = gsl::narrow<size_t>(std::distance(end_code.begin(), ec_it));

    auto const start_codes = implicit_cast<big_uint16_buf_t>(offset, bytes, seg_count);
    auto const start_code = *start_codes[segment_index];

    if (code_point < start_code) {
        return glyph_id{};
    }

    auto const id_deltas = implicit_cast<big_uint16_buf_t>(offset, bytes, seg_count);
    auto id_delta = *id_deltas[segment_index];

    auto const id_range_offsets = implicit_cast<big_uint16_buf_t>(offset, bytes, seg_count);
    auto id_range_offset = static_cast<size_t>(*id_range_offsets[segment_index]);

    if (id_range_offset == 0) {
        id_delta += code_point;
        return glyph_id{id_delta};
    }

    auto const glyph_id_array_count = (bytes.size() - offset) / sizeof(big_uint16_buf_t);
    auto const glyph_id_array = implicit_cast<big_uint16_buf_t>(offset, bytes, glyph_id_array_count);

    id_range_offset /= 2;
    id_range_offset += code_point;
    id_range_offset -= start_code;
    id_range_offset -= seg_count;
    id_range_offset += i;

    return glyph_id{glyph_id_array.at(id_range_offset)};
}

[[nodiscard]] inline glyph_id otype_cmap_find_map_6(std::span<std::byte const> over_sized_bytes, char32_t code_point)
{
    struct header_type {
        big_uint16_buf_t format;
        big_uint16_buf_t length;
        big_uint16_buf_t language;
        big_uint16_buf_t first_code;
        big_uint16_buf_t entry_count;
    };

    if (code_point > 0xffff) {
        return glyph_id{};
    }

    auto offset = 0_uz;
    auto const& header = implicit_cast<header_type>(offset, over_sized_bytes);
    hi_axiom(*header.format == 6);
    auto const bytes = hi_check_subspan(over_sized_bytes, 0, *header.length);

    auto const first_code = *header.entry_count;
    if (code_point < first_code) {
        return glyph_id{};
    }

    auto const index = static_cast<size_t>(code_point - first_code);
    auto const entry_count = *header.entry_count;
    if (index >= entry_count) {
        return glyph_id{};
    }

    auto const entries = implicit_cast<big_uint16_buf_t>(offset, bytes, entry_count);
    return glyph_id{*entries[index]};
}

[[nodiscard]] inline font_id otype_cmap_find_map_12(std::span<std::byte const> over_sized_bytes, char32_t code_point)
{
    struct header_type {
        big_uint16_buf_t format;
        big_uint16_buf_t reserved;
        big_uint32_buf_t length;
        big_uint32_buf_t language;
        big_uint32_buf_t num_groups;
    };

    struct entry_type {
        big_uint32_buf_t start_char_code;
        big_uint32_buf_t end_char_code;
        big_uint32_buf_t start_glyph_id;
    };

    auto offset = 0_uz;
    auto const& header = implicit_cast<header_type>(offset, over_sized_bytes);
    hi_axiom(*header.format == 12);
    auto const bytes = hi_check_subspan(over_sized_bytes, 0, *header.length);

    auto const entries = implicit_cast<entry_type>(offset, bytes, *header.num_groups);

    auto const it = std::lower_bound(entries.begin(), entries.end(), code_point, [](auto& entry, auto& value) {
        return *entry.end_char_code < value;
    });
    if (it == entries.end()) {
        return glyph_id{};
    }

    auto id = *entry.start_glyph_id;
    id += code_point;
    id -= *entry.start_char_code;
    return glyph_id{id};
}

/** Lookup a code-point in a sub-map.
 *
 * @param bytes A span arround the complete sub-map.
 * @param code_point The code-point to lookup.
 * @return The glyph_id found, or empty on failure.
 */
[[nodiscard]] inline glyph_id otype_cmap_find_map(std::span<std::byte const> bytes, char32_t code_point)
{
    // The first 16 bits of a cmap sub-table always contain the format.
    auto const format = *implicit_cast<big_uint16_buf_t>(bytes);

    switch (format) {
    case 4:
        return otype_cmap_find_map_4(bytes, code_point);
    case 6:
        return otype_cmap_find_map_6(bytes, code_point);
    case 12:
        return otype_cmap_find_map_12(bytes, code_point);
    default:
        return glyph_id{};
    }
}

/** Check if a 'cmap' sub-map is valid for lookup of a code-point.
 *
 * @param platform_id The platform-id of a sub-map.
 * @param platform_specific_id The platform-specific-id of a sub-map.
 * @param compatibility The compatibility type of the code-point being looked up.
 * @return true if the map is valid for this code-point.
 */
[[nodiscard]] inline bool otype_cmap_find_match_map(uint32_t platform_id, uint32_t platform_specific_id, unicode_compatibility compatibility) noexcept
{
    auto const platform = (platform_id << 16) | platform_specific_id;

    switch (platform) {
    case 0x0000'0003: // Unicode / Unicode 2.0 BMP
        return compatibility <= unicode_compatibility::BMP;
    case 0x0000'0004: // Unicode / Unicode 2.0 full reportoire
        return true;
    case 0x0002'0000: // ISO / ASCII
        return compatibility <= unicode_compatibility::ASCII;
    case 0x0002'0001: // ISO / ISO 10646
        return true;
    case 0x0002'0002: // ISO / ISO 8859-1
        return compatibility <= unicode_compatibility::Latin_1;
    case 0x0003'0001: // Windows / Unicode BMP
        return compatibility <= unicode_compatibility::BMP;
    case 0x0003'000a: // Windows / Unicode full reportoire
        return true;
    default:
        return false;
    }
}

/** Find a character in the 'cmap'-table of a open-type font file.
 *
 * @param bytes A span arround the complete 'cmap'-table.
 * @param code_point The code-point to look up in the table.
 * @return The glyph_id found, or empty on failure.
 */
[[nodiscard]] inline glyph_id otype_cmap_find(std::span<std::byte const> bytes, char32_t code_point)
{
    struct header_type {
        big_uint16_buf_t version;
        big_uint16_buf_t num_tables;
    };

    struct entry_type {
        big_uint16_buf_t platform_id;
        big_uint16_buf_t platform_specific_id;
        big_uint32_buf_t offset;
    };

    std::size_t offset = 0;

    auto const& header = implicit_cast<header_type>(offset, bytes);
    hi_check(*header.version == 0, "CMAP version is not 0");

    auto const compatiblity = hi::compatibility(code_point);
    auto const entries = implicit_cast<entry_type>(offset, bytes, *header.num_tables);
    while (auto const& entry : enties) {
        if (otype_cmap_find_match_map(*entry->platform_id, *entry->platform_specific_id, compatiblity)) {
            if (auto const glyph_id = otype_cmap_find_map(bytes.subspan(*entries->offset), code_point)) {
                return glyph_id;
            }
        }
    }

    return glyph_id{};
}


}} // namespace hi::v1
