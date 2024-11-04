// Copyright Take Vos 2024.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "otype_error.hpp"
#include "otype_script_list.hpp"
#include "otype_feature_list.hpp"
#include "otype_lookup_list.hpp"
#include "otype_coverage.hpp"
#include "../i18n/i18n.hpp"
#include "../geometry/geometry.hpp"
#include "../parser/parser.hpp"
#include "../utility/utility.hpp"
#include <cstddef>
#include <span>

hi_export_module(hikogui.font.otype_GSUB);

hi_export namespace hi::inline v1 {
enum class otype_GDEF_glyph_class : uint16_t {
    unknown = 0,
    base = 1,
    ligature = 2,
    mark = 3,
    component = 4,
};

[[nodiscard]] inline otype_GDEF_glyph_class otype_GDEF_search_class(std::span<std::byte const> bytes, glyph_id id)
{
    struct header_type {
        big_uint16_buf_t major_version;
        big_uint16_buf_t minor_version;
        big_uint16_buf_t glyph_class_def_offset;
        big_uint16_buf_t attach_list_offset;
        big_uint16_buf_t lig_caret_list_offset;
        big_uint16_buf_t mark_attach_class_def_offset;
    };

    auto offset = size_t{0};
    auto const header = implicit_cast<header_type>(offset, bytes);

    if (*header.major_version != 1 or
        (*header.minor_version != 0 and *header.minor_version != 2 and *header.minor_version != 3)) {
        throw otype_file_error("GDEF table version not supported");
    }

    auto const glyph_class_def_bytes = bytes.subspan(gsl::narrow_cast<size_t>(*header.glyph_class_def_offset) * 2);
    return otype_GDEF_glyph_class{otype_class_definition_table_search(glyph_class_def_bytes, id)};
}

[[nodiscard]] inline lean_vector<std::variant<int16_t,uint16_t>> otype_GDEF_search_lig_caret_list(std::span<std::byte const> bytes, glyph_id id)
{
    struct header_type {
        big_uint16_buf_t major_version;
        big_uint16_buf_t minor_version;
        big_uint16_buf_t glyph_class_def_offset;
        big_uint16_buf_t attach_list_offset;
        big_uint16_buf_t lig_caret_list_offset;
        big_uint16_buf_t mark_attach_class_def_offset;
    };

    struct lig_caret_list_type {
        big_uint16_buf_t coverage_offset;
        big_uint16_buf_t lig_glyph_count;
    };

    struct lig_glyph_type {
        big_uint16_buf_t caret_count;
    };

    struct caret_value_type1 {
        big_uint16_buf_t format;
        big_int16_buf_t coordinate;
    };

    struct caret_value_type2 {
        big_uint16_buf_t format;
        big_uint16_buf_t caret_value_point_index;
    };

    struct caret_value_type3 {
        big_uint16_buf_t format;
        big_int16_buf_t coordinate;
        big_uint16_buf_t device_table_offset;
    };

    auto offset = size_t{0};
    auto const header = implicit_cast<header_type>(offset, bytes);

    if (*header.major_version != 1 or
        (*header.minor_version != 0 and *header.minor_version != 2 and *header.minor_version != 3)) {
        throw otype_file_error("GDEF table version not supported");
    }

    auto const lig_caret_list_bytes = bytes.subspan(gsl::narrow_cast<size_t>(*header.lig_caret_list_offset) * 2);
    offset = 0;
    auto const lig_caret_list_header = implicit_cast<lig_caret_list_type>(offset, lig_caret_list_bytes);

    auto const coverage_bytes = lig_caret_list_bytes.subspan(gsl::narrow_cast<size_t>(*lig_caret_list_header.coverage_offset) * 2);
    auto const coverage_index = otype_coverage_search(coverage_bytes, id);
    if (not coverage_index) {
        return {};
    }

    auto const lig_glyph_offsets = implicit_cast<big_uint16_buf_t>(offset, lig_caret_list_bytes, *lig_caret_list_header.lig_glyph_count);
    auto const lig_glyph_offset = *lig_caret_offsets[*coverage_index];

    offset = 0;
    auto const lig_glyph_bytes = lig_caret_list_bytes.subspan(gs::narrow_cast<size_t>(lig_glyph_offset) * 2);
    auto const lig_glyph = implicit_cast<lig_glyph_type>(offset, lig_caret_bytes);
    auto const caret_value_offsets = implicit_cast<big_uint16_buf_t>(offset, lig_caret_bytes, *lig_glyph.caret_count);

    auto r = lean_vector<std::variant<int16_t,uint16_t>>{};
    r.reserve(*lig_glyph.caret_count);

    for (auto const caret_value_offset : caret_value_offsets) {
        auto const caret_value_bytes = lig_caret_bytes.subspan(gsl::narrow_cast<size_t>(caret_value_offset) * 2);
        offset = 0;
        auto const caret_value_format = implicit_cast<big_uint16_buf_t>(offset, caret_value_bytes);
        if (*caret_value_format == 1) {
            auto const caret_value = implicit_cast<caret_value_type1>(offset, caret_value_bytes);
            r.push_back(caret_value.coordinate);

        } else if (*caret_value_format == 2) {
            auto const caret_value = implicit_cast<caret_value_type2>(offset, caret_value_bytes);
            r.push_back(caret_value.caret_value_point_index);

        } else if (*caret_value_format == 3) {
            auto const caret_value = implicit_cast<caret_value_type3>(offset, caret_value_bytes);
            r.push_back(caret_value.coordinate);

        } else {
            throw otype_file_error("caret value format not supported");
        }
    }
}

[[nodiscard]] inline std::optional<uint16_t> otype_GDEF_search_mark_glyph_set(std::span<std::byte const> bytes, uint16_t mark_filtering_set, glyph_id id)
{
    struct header_type {
        big_uint16_buf_t major_version;
        big_uint16_buf_t minor_version;
        big_uint16_buf_t glyph_class_def_offset;
        big_uint16_buf_t attach_list_offset;
        big_uint16_buf_t lig_caret_list_offset;
        big_uint16_buf_t mark_attach_class_def_offset;
        big_uint16_buf_t mark_glyph_set_offset;
    };

    struct mark_glyph_set_type {
        big_uint16_buf_t format;
        big_uint16_buf_t mark_glyph_count;
    };

    auto offset = size_t{0};
    auto const header = implicit_cast<header_type>(offset, bytes);

    if (*header.major_version != 1 or (*header.minor_version != 2 and *header.minor_version != 3)) {
        throw otype_file_error("GDEF table version not supported for mark_glyph_set_offset");
    }

    auto const mark_glyph_set_bytes = bytes.subspan(gsl::narrow_cast<size_t>(*header.mark_glyph_set_offset) * 2);
    offset = 0;
    auto const mark_glyph_set_header = implicit_cast<mark_glyph_set_type>(offset, mark_glyph_set_bytes);
    auto const mark_glyph_count = *mark_glyph_set_header.mark_glyph_count;
    auto const mark_glyph_set_offsets = implicit_cast<big_uint32_buf_t>(offset, mark_glyph_set_bytes, mark_glyph_count);

    if (mark_filtering_set >= mark_glyph_count) {
        throw otype_file_error("mark_filtering_set is out of range");
    }

    auto const mark_glyph_set_offset = *mark_glyph_set_offsets[mark_filtering_set];
    auto const mark_glyph_set_coverage_bytes = mark_glyph_set_bytes.subspan(mark_glyph_set_offset);
    return otype_coverage_search(mark_glyph_set_coverage_bytes, id);
}

enum class otype_lookup_flag : uint16_t {
    right_to_left = 0x0001,
    ignore_base_glyphs = 0x0002,
    ignore_ligatures = 0x0004,
    ignore_marks = 0x0008,
    use_mark_filtering_set = 0x0010,
    reserved = 0x00E0,
    mark_attachment_class_filter = 0xFF00,
};

[[nodiscard]] constexpr otype_lookup_flag operator&(otype_lookup_flag lhs, otype_lookup_flag rhs) noexcept
{
    return static_cast<otype_lookup_flag>(std::to_underlying(lhs) & std::to_underlying(rhs));
}

[[nodiscard]] constexpr otype_lookup_flag operator|(otype_lookup_flag lhs, otype_lookup_flag rhs) noexcept
{
    return static_cast<otype_lookup_flag>(std::to_underlying(lhs) | std::to_underlying(rhs));
}

[[nodiscard]] constexpr otype_lookup_flag operator^(otype_lookup_flag lhs, otype_lookup_flag rhs) noexcept
{
    return static_cast<otype_lookup_flag>(std::to_underlying(lhs) ^ std::to_underlying(rhs));
}

[[nodiscard]] constexpr otype_lookup_flag operator~(otype_lookup_flag lhs) noexcept
{
    return static_cast<otype_lookup_flag>(~std::to_underlying(lhs));
}

[[nodiscard]] constexpr bool to_bool(otype_lookup_flag value) noexcept
{
    return static_cast<bool>(std::to_underlying(value));
}

/** Filter a glyph based on the lookup flag.
 *
 * @param bytes The GDEF table.
 * @param id The glyph id to filter.
 * @param index The index of the glyph in a grapheme cluster, starting at 0
 *              (base glyph).
 * @param lookup_flag The lookup flag to filter the glyph with.
 * @param mark_filtering_set The mark filtering set to filter the glyph with
 *                           when the lookup flag has the use_mark_filtering_set
 *                           flag set.
 * @return True when the glyph should be filtered out.
 */
[[nodiscard]] inline bool
otype_GDEF_filter_glyph(std::span<std::byte const> bytes, glyph_id id, size_t index, uint16_t lookup_flag, uint16_t mark_filtering_set)
{
    auto const lookup_flag_ = otype_lookup_flag{lookup_flag};

    auto const glyph_class = [&]() {
        if (to_bool(lookup_flag_ & otype_lookup_flag::ignore_ligatures)) {
            return otype_GDEF_search_class(bytes, id);
        } else {
            return index == 0 ? otype_GDEF_glyph_class::base : otype_GDEF_glyph_class::mark;
        }
    }();

    if (to_bool(lookup_flag_ & otype_lookup_flag::ignore_base_glyphs) and glyph_class == otype_GDEF_glyph_class::base) {
        return true;
    }
    if (to_bool(lookup_flag_ & otype_lookup_flag::ignore_marks) and glyph_class == otype_GDEF_glyph_class::mark) {
        return true;
    }
    if (to_bool(lookup_flag_ & otype_lookup_flag::ignore_ligatures) and glyph_class == otype_GDEF_glyph_class::ligature) {
        return true;
    }
    if (to_bool(lookup_flag_ & otype_GSUB_lookup_flag::use_mark_filtering_set) and glyph_class == otype_GDEF_glyph_class::mark) {
        if (not otype_GDEF_search_mark_glyph_set(bytes, mark_filtering_set, id)) {
            // Skip all marks that are not in the mark filtering set.
            return true;
        }
    }

    return false;
}

} // namespace hi::inline v1
