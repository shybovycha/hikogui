// Copyright Take Vos 2024.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "ucd_bidi_classes.hpp"
#include "../container/container.hpp"
#include "../macros.hpp"
#include <gsl/gsl>
#include <span>
#include <vector>
#include <tuple>
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>

hi_export_module(hikogui.unicode : unicode_bidi);

hi_export namespace hi::inline v1 {
/** Determine the paragraph direction of a paragraph.
 *
 * @tparam rule_X5c If true, the function will end at the matching PDI.
 *                  Used from inside X5c when recursing.
 * @param directions The bidi classes of the characters in the paragraph.
 * @param mode The direction mode of the paragraph. Either L, R, or B (LTR-auto).
 */
template<bool rule_X5c>
[[nodiscard]] unicode_bidi_class unicode_bidi_P2(std::span<unicode_bidi_class const> directions, unicode_bidi_class mode) noexcept
{
    using enum unicode_bidi_class;

    if (mode == L or mode == R) {
        return mode;
    }

    auto isolate_level = size_t{0};
    for (auto direction : directions) {
        switch (direction) {
        case L:
        case AL:
        case R:
            if (isolate_level == 0) {
                return direction;
            }
            break;
        case LRI:
        case RLI:
        case FSI:
            ++isolate_level;
            break;
        case PDI:
            if (isolate_level > 0) {
                --isolate_level;
            } else if (rule_X5c) {
                // End at the matching PDI, when recursing for rule X5c.
                return mode;
            }
            break;
        default:;
        }
    }
    return mode;
}

/** Determine the paragraph embedding level of a paragraph.
 *
 * @param paragraph_bidi_class The paragraph direction.
 */
[[nodiscard]] int8_t unicode_bidi_P3(unicode_bidi_class paragraph_bidi_class) noexcept
{
    return static_cast<int8_t>(paragraph_bidi_class == unicode_bidi_class::AL or paragraph_bidi_class == unicode_bidi_class::R);
}

/** Determine explicit embedding levels and directions.
 *
 * @param directions The bidi classes of the characters in the paragraph.
 * @param embedding_levels The embedding levels of the characters in the paragraph.
 * @param paragraph_embedding_level The embedding level of the paragraph.
 */
constexpr void unicode_bidi_X1(
    std::span<unicode_bidi_class> directions,
    std::span<int8_t> embedding_levels,
    int8_t paragraph_embedding_level) noexcept
{
    assert(directions.size() == embedding_levels.size());

    using enum unicode_bidi_class;

    struct directional_status_type {
        int8_t embedding_level;
        unicode_bidi_class override_status;
        bool isolate_status;
    };

    constexpr int8_t max_depth = 125;

    auto next_even = [](int8_t x) -> int8_t {
        return (x % 2 == 0) ? x + 2 : x + 1;
    };

    auto next_odd = [](int8_t x) -> int8_t {
        return (x % 2 == 1) ? x + 2 : x + 1;
    };

    long long overflow_isolate_count = 0;
    long long overflow_embedding_count = 0;
    long long valid_isolate_count = 0;

    // X1.
    auto directional_status = hi::stack<directional_status_type, max_depth + 2>{};
    directional_status.emplace_back(paragraph_embedding_level, ON, false);

    for (auto i = size_t{0}; i != directions.size(); ++i) {
        auto const current_embedding_level = directional_status.back().embedding_level;
        auto const current_override_status = directional_status.back().override_status;
        auto const next_odd_embedding_level = next_odd(current_embedding_level);
        auto const next_even_embedding_level = next_even(current_embedding_level);

        auto RLI_implementation = [&] {
            embedding_levels[i] = current_embedding_level;
            if (current_override_status != ON) {
                directions[i] = current_override_status;
            }

            if (next_odd_embedding_level <= max_depth && overflow_isolate_count == 0 && overflow_embedding_count == 0) {
                ++valid_isolate_count;
                directional_status.emplace_back(next_odd_embedding_level, ON, true);
            } else {
                ++overflow_isolate_count;
            }
        };

        auto LRI_implementation = [&] {
            embedding_levels[i] = current_embedding_level;
            if (current_override_status != ON) {
                directions[i] = current_override_status;
            }

            if (next_even_embedding_level <= max_depth && overflow_isolate_count == 0 && overflow_embedding_count == 0) {
                ++valid_isolate_count;
                directional_status.emplace_back(next_even_embedding_level, ON, true);
            } else {
                ++overflow_isolate_count;
            }
        };

        switch (directions[i]) {
        case RLE: // X2. Explicit embeddings
            if (next_odd_embedding_level <= max_depth && overflow_isolate_count == 0 && overflow_embedding_count == 0) {
                directional_status.emplace_back(next_odd_embedding_level, ON, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case LRE: // X3. Explicit embeddings
            if (next_even_embedding_level <= max_depth && overflow_isolate_count == 0 && overflow_embedding_count == 0) {
                directional_status.emplace_back(next_even_embedding_level, ON, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case RLO: // X4. Explicit overrides
            if (next_odd_embedding_level <= max_depth && overflow_isolate_count == 0 && overflow_embedding_count == 0) {
                directional_status.emplace_back(next_odd_embedding_level, R, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case LRO: // X5. Explicit overrides
            if (next_even_embedding_level <= max_depth && overflow_isolate_count == 0 && overflow_embedding_count == 0) {
                directional_status.emplace_back(next_even_embedding_level, L, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case RLI: // X5a. Isolates
            RLI_implementation();
            break;

        case LRI: // X5b. Isolates
            LRI_implementation();
            break;

        case FSI:
            { // X5c. Isolates
                // Direction mode is auto_LTR.
                auto const sub_paragraph_bidi_class = unicode_bidi_P2<true>(directions.subspan(i + 1), unicode_bidi_class::B);
                auto const sub_paragraph_embedding_level = unicode_bidi_P3(sub_paragraph_bidi_class);
                if (sub_paragraph_embedding_level == 0) {
                    LRI_implementation();
                } else {
                    RLI_implementation();
                }
            }
            break;

        case PDI: // X6a. Terminating Isolates
            if (overflow_isolate_count > 0) {
                --overflow_isolate_count;
            } else if (valid_isolate_count == 0) {
                // Mismatched PDI, do nothing.
                ;
            } else {
                overflow_embedding_count = 0;
                while (directional_status.back().isolate_status == false) {
                    directional_status.pop_back();
                }
                directional_status.pop_back();
                --valid_isolate_count;
            }

            embedding_levels[i] = directional_status.back().embedding_level;
            if (directional_status.back().override_status != ON) {
                directions[i] = directional_status.back().override_status;
            }
            break;

        case PDF: // X7. Terminating Embeddings and Overrides
            if (overflow_isolate_count > 0) {
                // PDF is in scope of isolate, wait until the isolate is terminated.
                ;
            } else if (overflow_embedding_count > 0) {
                --overflow_embedding_count;
            } else if (directional_status.back().isolate_status == false and directional_status.size() >= 2) {
                directional_status.pop_back();
            } else {
                // PDF does not match embedding character.
            }
            break;

        case B: // X8. End of Paragraph
            embedding_levels[i] = paragraph_embedding_level;
            return;

        case BN: // X6. Ignore
            break;

        default: // X6
            embedding_levels[i] = current_embedding_level;
            if (current_override_status != ON) {
                directions[i] = current_override_status;
            }
        }
    }
}

/** Ignore explicit embeddings and overrides.
 *
 * @param direction The bidi class of the character.
 * @return True if the character is an explicit embedding or override
 *         that should be ignored according to X9.
 */
[[nodiscard]] constexpr bool unicode_bidi_X9(unicode_bidi_class direction) noexcept
{
    using enum unicode_bidi_class;

    switch (direction) {
    case RLE:
    case LRE:
    case RLO:
    case LRO:
    case PDF:
    case BN:
        return true;
    default:
        return false;
    }
}

/** Determine level-runs.
 *
 * @param embedding_levels The embedding levels of the characters in the paragraph.
 * @return A vector of run-lengths of embedding-levels.
 */
[[nodiscard]] constexpr std::vector<std::pair<size_t, size_t>> unicode_bidi_BD7(std::span<int8_t> embedding_levels) noexcept
{
    std::vector<unicode_bidi_level_run> r;

    if (embedding_levels.empty()) {
        return r;
    }

    auto embedding_level = embedding_levels[0];
    auto run_start = size_t{0};
    for (auto i = size_t{1}; i != embedding_levels.size(); ++i) {
        if (embedding_levels[i] != embedding_level) {
            embedding_level = embedding_levels[i];
            r.emplace_back(run_start, i);
            run_start = i;
        }
    }
    r.emplace_back(run_start, embedding_levels.size());

    return r;
}

[[nodiscard]] constexpr unicode_bidi_class
unicode_bidi_BD13_first_class(std::span<unicode_bidi_class const> directions, size_t first, size_t last)
{
    assert(first < last);
    assert(last <= directions.size());

    for (auto i = first; i != last; ++i) {
        if (not unicode_bidi_X9(directions[i])) {
            return directions[i];
        }
    }
    return directions[first];
}

[[nodiscard]] constexpr unicode_bidi_class
unicode_bidi_BD13_last_class(std::span<unicode_bidi_class const> directions, size_t first, size_t last)
{
    assert(first < last);
    assert(last <= directions.size());

    for (auto i = last; i != first; --i) {
        if (not unicode_bidi_X9(directions[i - 1])) {
            return directions[i - 1];
        }
    }
    return directions[last - 1];
}

/** Determine isolated run sequences.
 *
 * @param directions The bidi classes of the characters in the paragraph.
 * @param runs The run-lengths of embedding-levels.
 * @return A vector of isolated run sequences.
 */
[[nodiscard]] constexpr std::vector<std::vector<std::pair<size_t, size_t>>>
unicode_bidi_BD13(std::span<unicode_bidi_class const> directions, sequence runs) noexcept
{
    auto run_has_isolate_initiator = [&](auto const& run) {
        return is_isolate_starter(unicode_bidi_BD13_first_class(directions, run.first, run.second));
    };
    auto sequence_has_isolate_initiator = [&](auto const& sequence) {
        return run_has_isolate_initiator(sequence.back());
    };
    auto run_has_PDI = [&](auto const& run) {
        return unicode_bidi_BD13_last_class(directions, run.first, run.second) == unicode_bidi_class::PDI;
    };

    std::vector<std::vector<std::pair<size_t, size_t>>> r;
    std::reverse(runs.begin(), runs.end());
    while (not runs.empty()) {
        auto sequence = std::vector<std::pair<size_t, size_t>>({runs.back()});
        runs.pop_back();

        while (sequence_has_isolate_initiator(sequence) and not runs.empty()) {
            // Search for matching PDI in the run_levels. This should have the same embedding level.
            auto isolation_level = 1;
            for (auto it = std::rbegin(runs); it != std::rend(runs); ++it) {
                if (run_has_PDI(*it) and --isolation_level == 0) {
                    sequence.push_back(*it);
                    runs.erase(std::next(it).base());
                    break;
                }
                if (run_has_isolate_initiator(*it)) {
                    ++isolation_level;
                }
            }

            if (isolation_level != 0) {
                // No PDI that matches the isolate initiator of this isolated run sequence.
                break;
            }
        }

        r.push_back(std::move(sequence));
    }

    return r;
}
}
