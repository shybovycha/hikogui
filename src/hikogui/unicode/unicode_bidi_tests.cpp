// Copyright Take Vos 2020-2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#include "unicode_bidi.hpp"
#include "../file/file.hpp"
#include "../path/path.hpp"
#include "../utility/utility.hpp"
#include "../algorithm/algorithm.hpp"
#include <hikotest/hikotest.hpp>
#include <iostream>
#include <string>
#include <string_view>
#include <span>
#include <format>
#include <ranges>

TEST_SUITE(unicode_bidi) {

struct unicode_bidi_test {
    std::vector<int> levels;
    std::vector<int> reorder;
    int line_nr;

    std::vector<hi::unicode_bidi_class> input;
    bool test_for_LTR = false;
    bool test_for_RTL = false;
    bool test_for_auto = false;

    [[nodiscard]] unicode_bidi_test(std::vector<int> const& levels, std::vector<int> const& reorder, int line_nr) noexcept :
        levels(levels), reorder(reorder), line_nr(line_nr)
    {
    }

    [[nodiscard]] std::vector<hi::detail::unicode_bidi_char_info> get_input() const noexcept
    {
        auto r = std::vector<hi::detail::unicode_bidi_char_info>{};
        auto index = 0;
        for (auto cls : input) {
            r.emplace_back(index++, cls);
        }
        return r;
    }

    [[nodiscard]] std::vector<hi::unicode_bidi_class> get_paragraph_directions() const noexcept
    {
        auto r = std::vector<hi::unicode_bidi_class>{};

        if (test_for_LTR) {
            r.push_back(hi::unicode_bidi_class::L);
        }
        if (test_for_RTL) {
            r.push_back(hi::unicode_bidi_class::R);
        }
        if (test_for_auto) {
            r.push_back(hi::unicode_bidi_class::B);
        }

        return r;
    }
};

[[nodiscard]] static std::vector<int> parse_bidi_test_levels(std::string_view line) noexcept
{
    auto r = std::vector<int>{};
    for (auto const value : hi::split_string(hi::strip(line), " ")) {
        if (value == "x") {
            r.push_back(-1);
        } else {
            r.push_back(*hi::from_string<int>(value));
        }
    }
    return r;
}

[[nodiscard]] static std::vector<int> parse_bidi_test_reorder(std::string_view line) noexcept
{
    auto r = std::vector<int>{};
    for (auto const value : hi::split_string(hi::strip(line), " ")) {
        if (value.empty()) {
            ;
        } else if (value == "x") {
            r.push_back(-1);
        } else {
            r.push_back(*hi::from_string<int>(value));
        }
    }
    return r;
}

[[nodiscard]] static unicode_bidi_test parse_bidi_test_data_line(
    std::string_view line,
    std::vector<int> const& levels,
    std::vector<int> const& reorder,
    int level_nr) noexcept
{
    auto r = unicode_bidi_test{levels, reorder, level_nr};

    auto line_s = hi::split_string(line, ";");

    for (auto bidi_class_str : hi::split_string(hi::strip(line_s[0]), " ")) {
        r.input.push_back(hi::unicode_bidi_class_from_string(bidi_class_str));
    }

    auto bitset = hi::from_string<int>(hi::strip(line_s[1]), 16);
    r.test_for_auto = (*bitset & 1) != 0;
    r.test_for_LTR = (*bitset & 2) != 0;
    r.test_for_RTL = (*bitset & 4) != 0;

    return r;
}

hi::generator<unicode_bidi_test> parse_bidi_test(int test_line_nr = -1)
{
    auto const view = hi::file_view(hi::library_test_data_dir() / "BidiTest.txt");
    auto const test_data = as_string_view(view);

    auto levels = std::vector<int>{};
    auto reorder = std::vector<int>{};

    int line_nr = 1;
    for (auto const line_view : std::views::split(test_data, std::string_view{"\n"})) {
        auto const line = hi::strip(std::string_view{line_view.begin(), line_view.end()});
        if (line.empty() || line.starts_with("#")) {
            // Comment and empty lines.
        } else if (line.starts_with("@Levels:")) {
            levels = parse_bidi_test_levels(line.substr(8));
        } else if (line.starts_with("@Reorder:")) {
            reorder = parse_bidi_test_reorder(line.substr(9));
        } else {
            auto data = parse_bidi_test_data_line(line, levels, reorder, line_nr);
            if (test_line_nr == -1 || line_nr == test_line_nr) {
                co_yield data;
            }
        }

        if (line_nr == test_line_nr) {
            break;
        }

        line_nr++;
    }
}

[[nodiscard]] static char32_t get_code_point(hi::unicode_bidi_class x) noexcept
{
    switch (x) {
    case hi::unicode_bidi_class::L:
        return U'a'; // Left-to-right
    case hi::unicode_bidi_class::R:
        return U'\u05D0'; // Right-to-left
    case hi::unicode_bidi_class::EN:
        return U'\u06F0'; // Extended number
    case hi::unicode_bidi_class::AN:
        return U'\u0660'; // Arabic number
    case hi::unicode_bidi_class::AL:
        return U'\u0608'; // Arabic letter
    case hi::unicode_bidi_class::NSM:
        return U'\u0300'; // Non-spacing mark
    case hi::unicode_bidi_class::CS:
        return U','; // Common separator
    case hi::unicode_bidi_class::ES:
        return U'+'; // European separator
    case hi::unicode_bidi_class::ET:
        return U'$'; // European terminator
    case hi::unicode_bidi_class::ON:
        return U'!'; // Other neutral
    case hi::unicode_bidi_class::BN:
        return U'\0'; // Boundary neutral
    case hi::unicode_bidi_class::S:
        return U'\t'; // Segment separator
    case hi::unicode_bidi_class::WS:
        return U' '; // White space
    case hi::unicode_bidi_class::B:
        return U'\n'; // Paragraph separator
    case hi::unicode_bidi_class::RLO:
        return U'\u202E'; // Right-to-left override
    case hi::unicode_bidi_class::RLE:
        return U'\u202B'; // Right-to-left embedding
    case hi::unicode_bidi_class::LRO:
        return U'\u202D'; // Left-to-right override
    case hi::unicode_bidi_class::LRE:
        return U'\u202A'; // Left-to-right embedding
    case hi::unicode_bidi_class::PDF:
        return U'\u202C'; // Pop directional formatting
    case hi::unicode_bidi_class::LRI:
        return U'\u2066'; // Left-to-right isolate
    case hi::unicode_bidi_class::RLI:
        return U'\u2067'; // Right-to-left isolate
    case hi::unicode_bidi_class::FSI:
        return U'\u2068'; // First strong isolate
    case hi::unicode_bidi_class::PDI:
        return U'\u2069'; // Pop directional isolate
    }
    std::unreachable();
}

TEST_CASE(bidi_test)
{
    for (auto test : parse_bidi_test()) {
        for (auto paragraph_direction : test.get_paragraph_directions()) {

            auto const embedding_levels_paragraphs = hi::unicode_bidi_get_embedding_levels(test.input.begin(), test.input.end(), paragraph_direction, get_code_point);
            auto const line_lengths = std::vector<size_t>{test.input.size()};
            auto const embedding_levels = hi::unicode_bidi_L1(line_lengths, embedding_levels_paragraphs.begin(), test.input.begin(), [](auto x) { return x; });

            // We are using the index from the iterator to find embedded levels
            // in input-order. We ignore all elements that where removed by X9.
            REQUIRE(embedding_levels.size() == test.levels.size());
            for (auto i = 0; i != embedding_levels.size(); ++i) {
                if (test.levels[i] != -1) {
                    REQUIRE(embedding_levels[i] == test.levels[i]);
                }
            }

            //auto const display_order = hi::unicode_bidi_to_display_order(
            //    line_lengths,
            //    embedding_levels_paragraphs.begin(),
            //    test.input.begin(),
            //    get_code_point);
//
            //auto index = 0;
            //for (auto it = first; it != last; ++it, ++index) {
            //    auto const expected_input_index = test.reorder[index];
//
            //    REQUIRE((expected_input_index == -1 or expected_input_index == it->index));
            //}
        }

#ifndef NDEBUG
        //if (test.line_nr > 10'000) {
        //    break;
        //}
#endif
    }
}

struct unicode_bidi_character_test {
    int line_nr;
    std::vector<char32_t> characters;
    hi::unicode_bidi_class paragraph_direction;
    hi::unicode_bidi_class resolved_paragraph_direction;
    std::vector<int> resolved_levels;
    std::vector<int> resolved_order;

    struct input_character {
        char32_t code_point;
        int index;
    };

    [[nodiscard]] std::vector<input_character> get_input() const noexcept
    {
        auto r = std::vector<input_character>{};

        int index = 0;
        for (auto const c : characters) {
            r.emplace_back(c, index++);
        }

        return r;
    }
};

[[nodiscard]] static unicode_bidi_character_test parse_bidi_character_test_line(std::string_view line, int line_nr)
{
    auto const split_line = hi::split_string(line, ";");
    auto const hex_characters = hi::split_string(split_line[0], " ");
    auto const paragraph_direction = hi::from_string<int>(split_line[1]);
    auto const resolved_paragraph_direction = hi::from_string<int>(split_line[2]);
    auto const int_resolved_levels = hi::split_string(split_line[3], " ");
    auto const int_resolved_order = hi::split_string(split_line[4], " ");

    auto r = unicode_bidi_character_test{};
    r.line_nr = line_nr;
    std::transform(begin(hex_characters), end(hex_characters), std::back_inserter(r.characters), [](auto const& x) {
        return hi::char_cast<char32_t>(*hi::from_string<uint32_t>(x, 16));
    });

    r.paragraph_direction = paragraph_direction == 0 ? hi::unicode_bidi_class::L :
        paragraph_direction == 1                     ? hi::unicode_bidi_class::R :
                                                       hi::unicode_bidi_class::ON;

    r.resolved_paragraph_direction = resolved_paragraph_direction == 0 ? hi::unicode_bidi_class::L :
        resolved_paragraph_direction == 1                              ? hi::unicode_bidi_class::R :
                                                                         hi::unicode_bidi_class::ON;

    std::transform(
        begin(int_resolved_levels), end(int_resolved_levels), std::back_inserter(r.resolved_levels), [](auto const& x) {
            if (x == "x") {
                return -1;
            } else {
                return *hi::from_string<int>(x);
            }
        });

    std::transform(begin(int_resolved_order), end(int_resolved_order), std::back_inserter(r.resolved_order), [](auto const& x) {
        return *hi::from_string<int>(x);
    });

    return r;
}

hi::generator<unicode_bidi_character_test> parse_bidi_character_test(int test_line_nr = -1)
{
    auto const view = hi::file_view(hi::library_test_data_dir() / "BidiCharacterTest.txt");
    auto const test_data = as_string_view(view);

    int line_nr = 1;
    for (auto const line_view : std::views::split(test_data, std::string_view{"\n"})) {
        auto const line = hi::strip(std::string_view{line_view.begin(), line_view.end()});
        if (line.empty() || line.starts_with("#")) {
            // Comment and empty lines.
        } else {
            auto data = parse_bidi_character_test_line(line, line_nr);
            if (test_line_nr == -1 || line_nr == test_line_nr) {
                co_yield data;
            }
        }

        if (line_nr == test_line_nr) {
            break;
        }

        line_nr++;
    }
}

TEST_CASE(bidi_character_test)
{
    for (auto test : parse_bidi_character_test()) {
        auto test_parameters = hi::unicode_bidi_context{};
        test_parameters.enable_mirrored_brackets = true;
        test_parameters.enable_line_separator = true;
        test_parameters.remove_explicit_embeddings = false;
        // clang-format off
        test_parameters.direction_mode =
            test.paragraph_direction == hi::unicode_bidi_class::L ? hi::unicode_bidi_context::mode_type::LTR :
            test.paragraph_direction == hi::unicode_bidi_class::R ? hi::unicode_bidi_context::mode_type::RTL :
            hi::unicode_bidi_context::mode_type::auto_LTR;
        // clang-format on

        auto input = test.get_input();
        auto first = begin(input);
        auto last = end(input);

        auto const [new_last, paragraph_directions] = hi::unicode_bidi(
            first,
            last,
            [](auto const& x) {
                return x.code_point;
            },
            [](auto& x, auto const& code_point) {
                x.code_point = code_point;
            },
            [](auto& x, auto bidi_class) {},
            test_parameters);

        last = new_last;
        // We are using the index from the iterator to find embedded levels
        // in input-order. We ignore all elements that where removed by X9.
        // for (auto it = first; it != last; ++it) {
        //    auto const expected_embedding_level = test.levels[it->index];
        //
        //    ASSERT_TRUE(expected_embedding_level == -1 || expected_embedding_level == it->embedding_level);
        //}

        REQUIRE(std::distance(first, last) == std::ssize(test.resolved_order));

        auto index = 0;
        for (auto it = first; it != last; ++it, ++index) {
            auto const expected_input_index = test.resolved_order[index];

            REQUIRE((expected_input_index == -1 or expected_input_index == it->index));
        }

#ifndef NDEBUG
        if (test.line_nr > 10'000) {
            break;
        }
#endif
    }
}

}; // TEST_SUITE(unicode_bidi)
