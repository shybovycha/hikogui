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
[[nodiscard]] constexpr sequence<> unicode_bidi_BD7(std::span<int8_t const> embedding_levels) noexcept
{
    sequence<> r;

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
[[nodiscard]] constexpr std::vector<sequence<>>
unicode_bidi_BD13(std::span<unicode_bidi_class const> directions, sequence<> runs) noexcept
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

    std::vector<sequence<>> r;
    std::reverse(runs.begin(), runs.end());
    while (not runs.empty()) {
        auto s = sequence<>({runs.back()});
        runs.pop_back();

        while (sequence_has_isolate_initiator(s) and not runs.empty()) {
            // Search for matching PDI in the run_levels. This should have the same embedding level.
            auto isolation_level = 1;
            for (auto it = std::rbegin(runs); it != std::rend(runs); ++it) {
                if (run_has_PDI(*it) and --isolation_level == 0) {
                    s.push_back(*it);
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

        r.push_back(std::move(s));
    }

    return r;
}


constexpr void unicode_bidi_W1(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class sos) noexcept
{
    using enum unicode_bidi_class;

    auto previous_bidi_class = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        if (unicode_bidi_X9(direction)) {
            continue;
        }

        if (direction == NSM) {
            switch (previous_bidi_class) {
            case LRI:
            case RLI:
            case FSI:
            case PDI:
                direction = ON;
                break;
            default:
                direction = previous_bidi_class;
                break;
            }
        }

        previous_bidi_class = direction;
    }
}

constexpr void unicode_bidi_W2(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class sos) noexcept
{
    using enum unicode_bidi_class;

    auto last_strong_direction = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        switch (direction) {
        case R:
        case L:
        case AL:
            last_strong_direction = direction;
            break;
        case EN:
            if (last_strong_direction == AL) {
                direction = AN;
            }
            break;
        default:;
        }
    }
}

constexpr void unicode_bidi_W3(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        if (direction == AL) {
            direction = R;
        }
    }
}

constexpr void unicode_bidi_W4(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    unicode_bidi_class *back1 = nullptr;
    unicode_bidi_class *back2 = nullptr;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        if (unicode_bidi_X9(direction)) {
            continue;
        }

        if (direction == EN and back2 != nullptr and *back2 == EN and back1 != nullptr and
            (*back1 == ES or *back1 == CS)) {
            *back1 = EN;
        }
        if (direction == AN and back2 != nullptr and *back2 == AN and back1 != nullptr and
            *back1 == CS) {
            *back1 = AN;
        }

        back2 = std::exchange(back1, &direction);
    }
}

constexpr void unicode_bidi_W5(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    auto const null = sequence.index_end();
    auto ET_start = null;
    auto starts_with_EN = false;

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        if (unicode_bidi_X9(direction)) {
            continue;
        }

        switch (direction) {
        case ET:
            if (starts_with_EN) {
                direction = EN;
            } else if (ET_start == null) {
                ET_start = it;
            }
            break;

        case EN:
            starts_with_EN = true;
            if (ET_start != null) {
                for (auto jt = ET_start; jt != it; ++jt) {
                    auto const j = *jt;

                    assert(j < directions.size());
                    if (not unicode_bidi_X9(directions[j])) {
                        directions[j] = EN;
                    }
                }
                ET_start = null;
            }
            break;

        default:
            starts_with_EN = false;
            ET_start = null;
        }
    }
}

constexpr void unicode_bidi_W6(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        if (direction == ET or direction == ES or direction == CS) {
            direction = ON;
        }
    }
}

constexpr void unicode_bidi_W7(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class sos) noexcept
{
    using enum unicode_bidi_class;

    auto last_strong_direction = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto &direction = directions[i];
        switch (direction) {
        case R:
        case L:
            last_strong_direction = direction;
            break;
        case EN:
            if (last_strong_direction == L) {
                direction = L;
            }
            break;
        default:;
        }
    }
}

constexpr void unicode_bidi_X10(
    std::span<unicode_bidi_class> directions,
    std::span<int8_t const> embedding_levels,
    int8_t paragraph_embedding_level) noexcept
{
    using enum unicode_bidi_class;
    
    auto const isolated_run_sequence_set = unicode_bidi_BD13(directions, unicode_bidi_BD7(embedding_levels));

    // All sos and eos calculations must be done before W*, N*, I* parts are executed,
    // since those will change the embedding levels of the characters outside of the
    // current isolated_run_sequence that the unicode_bidi_X10_sos_eos() depends on.
    std::vector<unicode_bidi_class> sos_list;
    std::vector<unicode_bidi_class> eos_list;
    for (auto& isolated_run_sequence : isolated_run_sequence_set) {
        auto const first_i = isolated_run_sequence.front().first;
        auto const last_i = isolated_run_sequence.back().second;
        auto const before_embedding_level = first_i == 0 ? paragraph_embedding_level : embedding_levels[first_i - 1];
        auto const after_embedding_level = last_i == embedding_levels.size() ? paragraph_embedding_level : embedding_levels[last_i];
        auto const first_embedding_level = std::max(embedding_levels[first_i], before_embedding_level);
        auto const last_embedding_level = std::max(embedding_levels[last_i - 1], after_embedding_level);
        sos_list.push_back((first_embedding_level % 2) == 1 ? R : L);
        eos_list.push_back((last_embedding_level % 2) == 1 ? R : L);
    }

    assert(sos_list.size() == isolated_run_sequence_set.size());
    assert(eos_list.size() == isolated_run_sequence_set.size());
    for (auto i = size_t{0}; i != isolated_run_sequence_set.size(); ++i) {
        auto const &isolated_run_sequence = isolated_run_sequence_set[i];
        auto const sos = sos_list[i];
        auto const eos = eos_list[i];

        unicode_bidi_W1(isolated_run_sequence, directions, sos);
        unicode_bidi_W2(isolated_run_sequence, directions, sos);
        unicode_bidi_W3(isolated_run_sequence, directions);
        unicode_bidi_W4(isolated_run_sequence, directions);
        unicode_bidi_W5(isolated_run_sequence, directions);
        unicode_bidi_W6(isolated_run_sequence, directions);
        unicode_bidi_W7(isolated_run_sequence, directions, sos);
        unicode_bidi_N0(isolated_run_sequence, context);
        unicode_bidi_N1(isolated_run_sequence);
        unicode_bidi_N2(isolated_run_sequence);
        unicode_bidi_I1_I2(isolated_run_sequence);
    }
}

}
