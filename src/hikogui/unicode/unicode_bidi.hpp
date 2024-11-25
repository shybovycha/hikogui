// Copyright Take Vos 2024.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "ucd_bidi_classes.hpp"
#include "ucd_bidi_mirroring_glyphs.hpp"
#include "ucd_bidi_paired_bracket_types.hpp"
#include "ucd_decompositions.hpp"
#include "../container/container.hpp"
#include "../macros.hpp"
#include <gsl/gsl>
#include <span>
#include <vector>
#include <tuple>
#include <algorithm>
#include <generator>
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
[[nodiscard]] constexpr unicode_bidi_class
unicode_bidi_P2(std::span<unicode_bidi_class const> directions, unicode_bidi_class mode) noexcept
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
[[nodiscard]] constexpr int8_t unicode_bidi_P3(unicode_bidi_class paragraph_bidi_class) noexcept
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

        auto& direction = directions[i];
        auto& embedding_level = embedding_levels[i];

        auto RLI_implementation = [&] {
            embedding_level = current_embedding_level;
            if (current_override_status != ON) {
                direction = current_override_status;
            }

            if (next_odd_embedding_level <= max_depth and overflow_isolate_count == 0 and overflow_embedding_count == 0) {
                ++valid_isolate_count;
                directional_status.emplace_back(next_odd_embedding_level, ON, true);
            } else {
                ++overflow_isolate_count;
            }
        };

        auto LRI_implementation = [&] {
            embedding_level = current_embedding_level;
            if (current_override_status != ON) {
                direction = current_override_status;
            }

            if (next_even_embedding_level <= max_depth and overflow_isolate_count == 0 and overflow_embedding_count == 0) {
                ++valid_isolate_count;
                directional_status.emplace_back(next_even_embedding_level, ON, true);
            } else {
                ++overflow_isolate_count;
            }
        };

        switch (direction) {
        case RLE: // X2. Explicit embeddings
            // X9. Retaining BNs and Explicit Formatting Characters.
            embedding_level = current_embedding_level;
            direction = BN;

            if (next_odd_embedding_level <= max_depth and overflow_isolate_count == 0 and overflow_embedding_count == 0) {
                directional_status.emplace_back(next_odd_embedding_level, ON, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case LRE: // X3. Explicit embeddings
            // X9. Retaining BNs and Explicit Formatting Characters.
            embedding_level = current_embedding_level;
            direction = BN;

            if (next_even_embedding_level <= max_depth and overflow_isolate_count == 0 and overflow_embedding_count == 0) {
                directional_status.emplace_back(next_even_embedding_level, ON, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case RLO: // X4. Explicit overrides
            // X9. Retaining BNs and Explicit Formatting Characters.
            embedding_level = current_embedding_level;
            direction = BN;

            if (next_odd_embedding_level <= max_depth and overflow_isolate_count == 0 and overflow_embedding_count == 0) {
                directional_status.emplace_back(next_odd_embedding_level, R, false);
            } else if (overflow_isolate_count == 0) {
                ++overflow_embedding_count;
            }
            break;

        case LRO: // X5. Explicit overrides
            // X9. Retaining BNs and Explicit Formatting Characters.
            embedding_level = current_embedding_level;
            direction = BN;

            if (next_even_embedding_level <= max_depth and overflow_isolate_count == 0 and overflow_embedding_count == 0) {
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

            embedding_level = directional_status.back().embedding_level;
            if (directional_status.back().override_status != ON) {
                direction = directional_status.back().override_status;
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

            // X9. Retaining BNs and Explicit Formatting Characters.
            embedding_level = directional_status.back().embedding_level;
            direction = BN;
            break;

        case B: // X8. End of Paragraph
            embedding_level = paragraph_embedding_level;
            return;

        case BN: // X6. Ignore
            // X9. Retaining BNs and Explicit Formatting Characters.
            embedding_level = current_embedding_level;
            break;

        default: // X6
            embedding_level = current_embedding_level;
            if (current_override_status != ON) {
                direction = current_override_status;
            }
        }
    }
}

constexpr void unicode_bidi_X9(std::span<unicode_bidi_class> directions)
{
    using enum unicode_bidi_class;

    for (auto& direction : directions) {
        switch (direction) {
        case RLE:
        case LRE:
        case RLO:
        case LRO:
        case PDF:
            direction = BN;
            break;
        default:;
        }
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
    using enum unicode_bidi_class;

    assert(first < last);
    assert(last <= directions.size());

    for (auto i = first; i != last; ++i) {
        if (directions[i] != BN) {
            return directions[i];
        }
    }
    return directions[first];
}

[[nodiscard]] constexpr unicode_bidi_class
unicode_bidi_BD13_last_class(std::span<unicode_bidi_class const> directions, size_t first, size_t last)
{
    using enum unicode_bidi_class;

    assert(first < last);
    assert(last <= directions.size());

    for (auto i = last; i != first; --i) {
        if (directions[i - 1] != BN) {
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
        return is_isolate_starter(unicode_bidi_BD13_last_class(directions, run.first, run.second));
    };
    auto sequence_has_isolate_initiator = [&](auto const& sequence) {
        return run_has_isolate_initiator(sequence.back());
    };
    auto run_has_PDI = [&](auto const& run) {
        return unicode_bidi_BD13_first_class(directions, run.first, run.second) == unicode_bidi_class::PDI;
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

constexpr void
unicode_bidi_W1(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class sos) noexcept
{
    using enum unicode_bidi_class;

    auto previous_bidi_class = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto& direction = directions[i];

        // X9. Retaining BNs and Explicit Formatting Characters.
        if (direction == BN) {
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

constexpr void
unicode_bidi_W2(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class sos) noexcept
{
    using enum unicode_bidi_class;

    auto last_strong_direction = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto& direction = directions[i];
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
        auto& direction = directions[i];
        if (direction == AL) {
            direction = R;
        }
    }
}

constexpr void unicode_bidi_W4(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    unicode_bidi_class* back1 = nullptr;
    unicode_bidi_class* back2 = nullptr;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto& direction = directions[i];

        // X9. Retaining BNs and Explicit Formatting Characters.
        if (direction == BN) {
            continue;
        }

        if (direction == EN and back2 != nullptr and *back2 == EN and back1 != nullptr and (*back1 == ES or *back1 == CS)) {
            *back1 = EN;
        }
        if (direction == AN and back2 != nullptr and *back2 == AN and back1 != nullptr and *back1 == CS) {
            *back1 = AN;
        }

        back2 = std::exchange(back1, &direction);
    }
}

constexpr void unicode_bidi_W5(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    enum class state_type { idle, found_BN, found_ET, found_EN };

    auto const null = sequence.index_end();

    auto state = state_type::idle;
    auto start = null;

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto const& direction = directions[i];

        switch (direction) {
        case BN:
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (state < state_type::found_BN) {
                state = state_type::found_BN;
                start = it;
            }
            break;

        case ET:
            if (state < state_type::found_ET) {
                state = state_type::found_ET;
                if (start == null) {
                    start = it;
                }
            }
            break;

        case EN:
            if (state < state_type::found_EN) {
                state = state_type::found_EN;
                if (start == null) {
                    start = it;
                }
            }
            break;

        default:
            if (state == state_type::found_EN) {
                assert(start != null);
                for (auto jt = start; jt != it; ++jt) {
                    directions[*jt] = EN;
                }
            }

            state = state_type::idle;
            start = null;
        }
    }

    if (state == state_type::found_EN) {
        assert(start != null);
        for (auto jt = start; jt != sequence.index_end(); ++jt) {
            directions[*jt] = EN;
        }
    }
}

constexpr void unicode_bidi_W6(sequence<> const& sequence, std::span<unicode_bidi_class> directions) noexcept
{
    using enum unicode_bidi_class;

    enum class state_type { idle, found_BN, found_ET_ES_CS };

    auto const null = sequence.index_end();

    auto state = state_type::idle;
    auto start = null;

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto const& direction = directions[i];

        switch (direction) {
        case BN:
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (state < state_type::found_BN) {
                state = state_type::found_BN;
                start = it;
            }
            break;

        case ET:
        case ES:
        case CS:
            if (state < state_type::found_ET_ES_CS) {
                state = state_type::found_ET_ES_CS;
                if (start == null) {
                    start = it;
                }
            }
            break;

        default:
            if (state == state_type::found_ET_ES_CS) {
                assert(start != null);
                for (auto jt = start; jt != it; ++jt) {
                    directions[*jt] = ON;
                }
            }

            state = state_type::idle;
            start = null;
        }
    }

    if (state == state_type::found_ET_ES_CS) {
        assert(start != null);
        for (auto jt = start; jt != sequence.index_end(); ++jt) {
            directions[*jt] = ON;
        }
    }
}

constexpr void
unicode_bidi_W7(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class sos) noexcept
{
    using enum unicode_bidi_class;

    auto last_strong_direction = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto& direction = directions[i];
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

using unicode_bidi_bracket_pair = std::pair<sequence<>::index_iterator, sequence<>::index_iterator>;

[[nodiscard]]
constexpr std::vector<unicode_bidi_bracket_pair> unicode_bidi_BD16(
    sequence<> const& sequence,
    std::span<unicode_bidi_class const> directions,
    std::span<unicode_bidi_paired_bracket_type const> brackets,
    std::span<char32_t const> code_points)
{
    struct bracket_start {
        hi::sequence<>::index_iterator it;
        char32_t mirrored_bracket;
    };

    using enum unicode_bidi_class;

    auto pairs = std::vector<unicode_bidi_bracket_pair>{};
    auto stack = hi::stack<bracket_start, 63>{};

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto const direction = directions[i];
        assert(i < brackets.size());
        auto const bracket_type = brackets[i];
        assert(i < code_points.size());
        auto const code_point = code_points[i];

        if (direction == ON) {
            switch (bracket_type) {
            case unicode_bidi_paired_bracket_type::o:
                if (stack.full()) {
                    // Stop processing
                    std::sort(pairs.begin(), pairs.end());
                    return pairs;

                } else {
                    // If there is a canonical equivalent of the opening bracket, find it's mirrored glyph
                    // to compare with the closing bracket.
                    auto mirrored_glyph = ucd_get_bidi_mirroring_glyph(code_point);
                    if (auto const canonical_equivalent = ucd_get_decomposition(code_point).canonical_equivalent()) {
                        assert(ucd_get_bidi_paired_bracket_type(*canonical_equivalent) == unicode_bidi_paired_bracket_type::o);

                        mirrored_glyph = ucd_get_bidi_mirroring_glyph(*canonical_equivalent);
                    }

                    stack.emplace_back(it, mirrored_glyph);
                }
                break;

            case unicode_bidi_paired_bracket_type::c:
                {
                    auto const canonical_equivalent = ucd_get_decomposition(code_point).canonical_equivalent();
                    auto jt = stack.end();
                    while (jt != stack.begin()) {
                        --jt;

                        if (jt->mirrored_bracket == code_point or
                            (canonical_equivalent and jt->mirrored_bracket == *canonical_equivalent)) {
                            pairs.emplace_back(jt->it, it);
                            stack.pop_back(jt);
                            break;
                        }
                    }
                }
                break;

            default:;
            }
        }
    }

    std::sort(pairs.begin(), pairs.end());
    return pairs;
}

[[nodiscard]] constexpr unicode_bidi_class unicode_bidi_N0_strong(unicode_bidi_class direction)
{
    using enum unicode_bidi_class;

    switch (direction) {
    case L:
        return L;
    case R:
    case EN:
    case AN:
        return R;
    default:
        return ON;
    }
}

[[nodiscard]] constexpr unicode_bidi_class unicode_bidi_N0_preceding_strong_type(
    std::span<unicode_bidi_class const> directions,
    sequence<>::index_iterator const& begin,
    sequence<>::index_iterator const& open_bracket,
    unicode_bidi_class sos)
{
    using enum unicode_bidi_class;

    auto it = open_bracket;
    while (it != begin) {
        auto const i = *--it;
        assert(i < directions.size());
        if (auto const direction = unicode_bidi_N0_strong(directions[i]); direction != ON) {
            return direction;
        }
    }

    return sos;
}

[[nodiscard]] constexpr unicode_bidi_class unicode_bidi_N0_enclosed_strong_type(
    unicode_bidi_bracket_pair const& pair,
    std::span<unicode_bidi_class const> directions,
    unicode_bidi_class embedding_direction)
{
    using enum unicode_bidi_class;

    auto opposite_direction = ON;
    for (auto it = pair.first + 1; it != pair.second; ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto const direction = unicode_bidi_N0_strong(directions[i]);
        if (direction == ON) {
            continue;
        }
        if (direction == embedding_direction) {
            return direction;
        }
        opposite_direction = direction;
    }

    return opposite_direction;
}

constexpr void unicode_bidi_N0(
    sequence<> const& sequence,
    std::span<unicode_bidi_class> directions,
    std::span<int8_t const> embedding_levels,
    std::span<unicode_bidi_paired_bracket_type const> brackets,
    std::span<char32_t const> code_points,
    unicode_bidi_class sos)
{
    using enum unicode_bidi_class;

    auto const bracket_pairs = unicode_bidi_BD16(sequence, directions, brackets, code_points);
    auto const first_i = sequence.front().first;
    assert(first_i < embedding_levels.size());
    auto const embedding_direction = embedding_levels[first_i] % 2 == 0 ? L : R;

    for (auto& pair : bracket_pairs) {
        auto pair_direction = unicode_bidi_N0_enclosed_strong_type(pair, directions, embedding_direction);
        if (pair_direction == ON) {
            continue;
        }

        if (pair_direction != embedding_direction) {
            pair_direction = unicode_bidi_N0_preceding_strong_type(directions, sequence.index_begin(), pair.first, sos);

            if (pair_direction == embedding_direction || pair_direction == ON) {
                pair_direction = embedding_direction;
            }
        }

        directions[*pair.first] = pair_direction;
        directions[*pair.second] = pair_direction;

        for (auto it = pair.first + 1; it != pair.second; ++it) {
            auto const i = *it;

            assert(i < code_points.size());
            if (ucd_get_bidi_class(code_points[i]) != NSM) {
                break;
            }

            assert(i < directions.size());
            directions[i] = pair_direction;
        }

        for (auto it = pair.second + 1; it != sequence.index_end(); ++it) {
            auto const i = *it;

            assert(i < code_points.size());
            if (ucd_get_bidi_class(code_points[i]) != NSM) {
                break;
            }

            assert(i < directions.size());
            directions[i] = pair_direction;
        }
    }
}

constexpr void unicode_bidi_N1(
    sequence<> const& sequence,
    std::span<unicode_bidi_class> directions,
    unicode_bidi_class sos,
    unicode_bidi_class eos)
{
    using enum unicode_bidi_class;

    enum class state_type { idle, found_BD, found_NI };

    auto const null = sequence.index_end();

    auto start = null;
    auto state = state_type::idle;
    auto direction_before_NI = sos;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto& direction = directions[i];

        switch (direction) {
        case BN:
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (state < state_type::found_BD) {
                state = state_type::found_BD;
                if (start == null) {
                    start = it;
                }
            }
            break;

        case B:
        case S:
        case WS:
        case ON:
        case FSI:
        case LRI:
        case RLI:
        case PDI:
            if (state < state_type::found_NI) {
                state = state_type::found_NI;
                if (start == null) {
                    start = it;
                }
            }
            break;

        default:
            if (state == state_type::found_NI) {
                auto const direction_after_NI = (direction == EN or direction == AN) ? R : direction;
                auto const same_direction = direction_before_NI == direction_after_NI;
                auto const direction_before_NI_is_strong = direction_before_NI == L or direction_before_NI == R;
                auto const direction_after_NI_is_strong = direction_after_NI == L or direction_after_NI == R;

                if (direction_before_NI_is_strong and direction_after_NI_is_strong and same_direction) {
                    assert(start != null);
                    for (auto jt = start; jt != it; ++jt) {
                        auto const j = *jt;

                        assert(j < directions.size());
                        directions[j] = direction_before_NI;
                    }
                }
            }

            state = state_type::idle;
            start = null;
            direction_before_NI = (direction == EN or direction == AN) ? R : direction;
        }
    }

    // Handle the case where the last character is a NI.
    if (state == state_type::found_NI) {
        auto const same_direction = direction_before_NI == eos;
        auto const direction_before_NI_is_strong = direction_before_NI == L or direction_before_NI == R;

        if (direction_before_NI_is_strong and same_direction) {
            assert(start != null);
            for (auto it = start; it != sequence.index_end(); ++it) {
                auto const i = *it;

                assert(i < directions.size());
                directions[i] = direction_before_NI;
            }
        }
    }
}

constexpr void
unicode_bidi_N2(sequence<> const& sequence, std::span<unicode_bidi_class> directions, unicode_bidi_class embedding_direction)
{
    using enum unicode_bidi_class;

    enum class state_type { idle, found_BN, found_NI };

    auto const null = sequence.index_end();

    auto start = null;
    auto state = state_type::idle;
    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;

        assert(i < directions.size());
        auto& direction = directions[i];

        switch (direction) {
        case BN:
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (state < state_type::found_BN) {
                state = state_type::found_BN;
                if (start == null) {
                    start = it;
                }
            }
            break;

        case B:
        case S:
        case WS:
        case ON:
        case FSI:
        case LRI:
        case RLI:
        case PDI:
            if (state < state_type::found_NI) {
                state = state_type::found_NI;
                if (start == null) {
                    start = it;
                }
            }
            break;

        default:
            if (state == state_type::found_NI) {
                assert(start != null);
                for (auto jt = start; jt != it; ++jt) {
                    auto const j = *jt;

                    assert(j < directions.size());
                    directions[j] = embedding_direction;
                }
            }

            state = state_type::idle;
            start = null;
        }
    }

    if (state == state_type::found_NI) {
        assert(start != null);
        for (auto jt = start; jt != sequence.index_end(); ++jt) {
            auto const j = *jt;

            assert(j < directions.size());
            directions[j] = embedding_direction;
        }
    }
}

constexpr void
unicode_bidi_I1_I2(sequence<> const& sequence, std::span<unicode_bidi_class> directions, std::span<int8_t> embedding_levels)
{
    using enum unicode_bidi_class;

    assert(directions.size() == embedding_levels.size());

    for (auto it = sequence.index_begin(); it != sequence.index_end(); ++it) {
        auto const i = *it;
        assert(i < directions.size());
        auto const& direction = directions[i];
        assert(i < embedding_levels.size());
        auto& embedding_level = embedding_levels[i];

        if ((embedding_level % 2) == 0) {
            // I1
            if (direction == R) {
                embedding_level += 1;
            } else if (direction == AN or direction == EN) {
                embedding_level += 2;
            }
        } else {
            // I2
            if (direction == L or direction == AN or direction == EN) {
                embedding_level += 1;
            }
        }
    }
}

constexpr std::pair<unicode_bidi_class, unicode_bidi_class> unicode_bidi_X10_sos_eos(
    std::span<unicode_bidi_class const> directions,
    std::span<int8_t const> embedding_levels,
    int8_t paragraph_embedding_level,
    size_t first_i,
    size_t last_i)
{
    using enum unicode_bidi_class;

    assert(first_i < last_i);
    auto const isolated_run_sequence_embedding_level = embedding_levels[first_i];

    auto const has_isolate_initiator = [&] {
        for (auto i = last_i; i != first_i; --i) {
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (directions[i - 1] == BN) {
                continue;
            }

            if (directions[i - 1] == RLI or directions[i - 1] == LRI or directions[i - 1] == FSI) {
                return true;
            }
            return false;
        }
        return false;
    }();

    auto const before_embedding_level = [&] {
        for (auto i = first_i; i != 0; --i) {
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (directions[i - 1] == BN) {
                continue;
            }

            return embedding_levels[i - 1];
        }
        return paragraph_embedding_level;
    }();

    auto const after_embedding_level = [&] {
        if (has_isolate_initiator) {
            // "and if there is none or the last character of the sequence
            // is an isolate initiator (lacking a matching PDI), with the
            // paragraph embedding level"
            return paragraph_embedding_level;
        }
        for (auto i = last_i; i != embedding_levels.size(); ++i) {
            // X9. Retaining BNs and Explicit Formatting Characters.
            if (directions[i] == BN) {
                continue;
            }

            return embedding_levels[i];
        }

        // "and if there is none or the last character of the sequence is an
        // isolate initiator (lacking a matching PDI), with the paragraph
        // embedding level"
        return paragraph_embedding_level;
    }();

    auto const sos = std::max(isolated_run_sequence_embedding_level, before_embedding_level) % 2 == 0 ? L : R;
    auto const eos = std::max(isolated_run_sequence_embedding_level, after_embedding_level) % 2 == 0 ? L : R;
    return {sos, eos};
}

constexpr void unicode_bidi_X10(
    std::span<unicode_bidi_class> directions,
    std::span<int8_t> embedding_levels,
    std::span<unicode_bidi_paired_bracket_type const> brackets,
    std::span<char32_t const> code_points,
    int8_t paragraph_embedding_level)
{
    using enum unicode_bidi_class;

    assert(directions.size() == embedding_levels.size());
    assert(directions.size() == brackets.size());
    assert(directions.size() == code_points.size());

    auto const isolated_run_sequence_set = unicode_bidi_BD13(directions, unicode_bidi_BD7(embedding_levels));

    // All sos and eos calculations must be done before I* parts are executed,
    // since those will change the embedding levels of the characters outside of the
    // current isolated_run_sequence that the unicode_bidi_X10_sos_eos() depends on.
    auto sos_eos = std::vector<std::pair<unicode_bidi_class, unicode_bidi_class>>{};
    sos_eos.reserve(isolated_run_sequence_set.size());
    for (auto& isolated_run_sequence : isolated_run_sequence_set) {
        sos_eos.push_back(unicode_bidi_X10_sos_eos(
            directions,
            embedding_levels,
            paragraph_embedding_level,
            isolated_run_sequence.front().first,
            isolated_run_sequence.back().second));
    }

    assert(sos_eos.size() == isolated_run_sequence_set.size());
    for (auto i = size_t{0}; i != isolated_run_sequence_set.size(); ++i) {
        auto const& isolated_run_sequence = isolated_run_sequence_set[i];
        auto const [sos, eos] = sos_eos[i];
        auto const embedding_level = embedding_levels[isolated_run_sequence.front().first];
        auto const embedding_direction = (embedding_level % 2) == 0 ? L : R;

        unicode_bidi_W1(isolated_run_sequence, directions, sos);
        unicode_bidi_W2(isolated_run_sequence, directions, sos);
        unicode_bidi_W3(isolated_run_sequence, directions);
        unicode_bidi_W4(isolated_run_sequence, directions);
        unicode_bidi_W5(isolated_run_sequence, directions);
        unicode_bidi_W6(isolated_run_sequence, directions);
        unicode_bidi_W7(isolated_run_sequence, directions, sos);
        unicode_bidi_N0(isolated_run_sequence, directions, embedding_levels, brackets, code_points, sos);
        unicode_bidi_N1(isolated_run_sequence, directions, sos, eos);
        unicode_bidi_N2(isolated_run_sequence, directions, embedding_direction);
        unicode_bidi_I1_I2(isolated_run_sequence, directions, embedding_levels);
    }
}

/** Reset the whitespace characters at the end of the line, segment and paragraph.
 *
 * @param original_directions The original bidi classes of the characters in the line.
 */
constexpr void unicode_bidi_L1(
    std::span<unicode_bidi_class const> original_directions,
    std::span<int8_t> embedding_levels,
    int8_t paragraph_embedding_level) noexcept
{
    using enum unicode_bidi_class;

    enum class state_type {
        idle,
        found_BN,
        found_WS_ISOLATE,
    };

    assert(original_directions.size() == embedding_levels.size());

    auto const null = original_directions.size();

    auto state = state_type::idle;
    auto start = null;

    auto preceding_character_level = paragraph_embedding_level; 
    for (auto i = size_t{0}; i != original_directions.size(); ++i) {
        auto const& direction = original_directions[i];
        auto& embedding_level = embedding_levels[i];

        switch (direction) {
        case RLE:
        case LRE:
        case RLO:
        case LRO:
        case PDF:
        case BN:
            // X9. Retaining BNs and Explicit Formatting Characters.
            // All Explicit Formatting Characters are added to the sequence
            // of whitespace characters preceding a segment separator, paragraph
            // separator, or end of the line.
            if (state < state_type::found_BN) {
                state = state_type::found_BN;
                if (start == null) {
                    start = i;
                }
            }

            // X9. Retaining BNs and Explicit Formatting Characters.
            // All Explicit Formatting Characters embedding level is set to the
            // previous character's embedding level.
            embedding_level = preceding_character_level;
            break;

        case WS:
        case FSI:
        case LRI:
        case RLI:
        case PDI:
            if (state < state_type::found_WS_ISOLATE) {
                state = state_type::found_WS_ISOLATE;
                if (start == null) {
                    start = i;
                }
            }
            break;

        case S:
        case B:
            if (state != state_type::idle) {
                assert(start != null);
                for (auto j = start; j != i; ++j) {
                    embedding_levels[j] = paragraph_embedding_level;
                }
            }
            embedding_level = paragraph_embedding_level;
            [[fallthrough]];
        default:
            state = state_type::idle;
            start = null;
        }

        preceding_character_level = embedding_level;
    }

    // Handle sequence of WS, FSI, LRI, RLI, PDI at the end of a line.
    if (state != state_type::idle) {
        assert(start != null);
        for (auto j = start; j != original_directions.size(); ++j) {
            embedding_levels[j] = paragraph_embedding_level;
        }
    }
}

[[nodiscard]] constexpr std::vector<size_t> unicode_bidi_L2(std::span<int8_t const> embedding_levels) noexcept
{
    auto highest = int8_t{0};
    auto lowest_odd = int8_t{127};
    for (auto const level : embedding_levels) {
        highest = std::max(highest, level);
        if (level % 2 == 1) {
            lowest_odd = std::min(lowest_odd, level);
        }
    }

    auto r = std::vector<size_t>{};
    r.reserve(embedding_levels.size());
    for (auto i = size_t{0}; i != embedding_levels.size(); ++i) {
        r.push_back(i);
    }

    if (highest < lowest_odd) {
        return r;
    }

    for (int8_t level = highest; level >= lowest_odd; --level) {
        auto const null = embedding_levels.size();
        auto sequence_start = null;
        for (auto i = size_t{0}; i != embedding_levels.size(); ++i) {
            auto const& embedding_level = embedding_levels[i];

            if (sequence_start == null) {
                if (embedding_level >= level) {
                    sequence_start = i;
                }

            } else if (embedding_level < level) {
                std::reverse(r.begin() + sequence_start, r.begin() + i);
                sequence_start = null;
            }
        }

        // Reverse the last sequence if it was not closed.
        if (sequence_start != null) {
            std::reverse(r.begin() + sequence_start, r.end());
        }
    }

    return r;
}

// unicode_bidi_L3() is not implemented due to the algorithm working on only
// the base code-point of each grapheme.

constexpr void unicode_bidi_L4(
    std::span<unicode_bidi_class const> directions,
    std::span<unicode_bidi_paired_bracket_type const> brackets,
    std::span<char32_t> code_points) noexcept
{
    using enum unicode_bidi_class;

    assert(directions.size() == brackets.size());
    assert(directions.size() == code_points.size());

    for (auto i = size_t{0}; i != directions.size(); ++i) {
        auto const& direction = directions[i];
        auto const& bracket = brackets[i];
        auto& code_point = code_points[i];

        if (bracket != unicode_bidi_paired_bracket_type::n and direction == R) {
            code_point = ucd_get_bidi_mirroring_glyph(code_point);
        }
    }
}

[[nodiscard]] constexpr std::vector<size_t> unicode_bidi_P1_split(std::span<unicode_bidi_class const> directions) noexcept
{
    auto r = std::vector<size_t>{};

    auto start = size_t{0};
    for (auto i = size_t{0}; i != directions.size(); ++i) {
        if (directions[i] == unicode_bidi_class::B) {
            r.emplace_back(i + 1 - start);
            start = i + 1;
        }
    }

    if (start != directions.size()) {
        r.emplace_back(directions.size() - start);
    }

    return r;
}

/** Apply the bidirectional algorithm at paragraph level, before line breaking.\
 *
 * @param directions The bidi classes of the characters in the paragraph.
 * @param embedding_levels The embedding levels of the characters in the
 *                         paragraph.
 * @param brackets The paired bracket types of the characters in the paragraph.
 * @param code_points The code points of the characters in the paragraph.
 * @param mode The mode for determining the paragraph direction. L: The
 *             paragraph direction is left-to-right. R: The paragraph direction
 *             is right-to-left. B: The paragraph direction is determined by the
 *             first strong character in the paragraph.
 * @return The paragraph embedding levels, one for each paragraph in the text.
 */
constexpr std::vector<int8_t> unicode_bidi_P1(
    std::span<unicode_bidi_class> directions,
    std::span<int8_t> embedding_levels,
    std::span<unicode_bidi_paired_bracket_type const> brackets,
    std::span<char32_t const> code_points,
    unicode_bidi_class mode) noexcept
{
    assert(directions.size() == embedding_levels.size());
    assert(directions.size() == brackets.size());
    assert(directions.size() == code_points.size());

    auto paragraph_sizes = unicode_bidi_P1_split(directions);
    auto r = std::vector<int8_t>{};
    r.reserve(paragraph_sizes.size());
    auto i = size_t{0};
    for (auto paragraph_size : paragraph_sizes) {
        auto const p_first = i;
        auto const p_last = i + paragraph_size;
        i += paragraph_size;

        assert(p_first < p_last);
        assert(p_last <= directions.size());

        auto const p_directions = directions.subspan(p_first, p_last - p_first);
        auto const p_embedding_levels = embedding_levels.subspan(p_first, p_last - p_first);
        auto const p_brackets = brackets.subspan(p_first, p_last - p_first);
        auto const p_code_points = code_points.subspan(p_first, p_last - p_first);

        auto const paragraph_direction = unicode_bidi_P2<false>(p_directions, mode);
        auto const paragraph_embedding_level = unicode_bidi_P3(paragraph_direction);
        r.push_back(paragraph_embedding_level);

        unicode_bidi_X1(p_directions, p_embedding_levels, paragraph_embedding_level);
        unicode_bidi_X9(p_directions);
        unicode_bidi_X10(p_directions, p_embedding_levels, p_brackets, p_code_points, paragraph_embedding_level);
    }

    return r;
}

struct unicode_bidi_phase1_result {
    std::vector<int8_t> embedding_levels;
    std::vector<unicode_bidi_class> original_directions;
    std::vector<unicode_bidi_class> resolved_directions;
    std::vector<unicode_bidi_paired_bracket_type> brackets;
};

[[nodiscard]] constexpr unicode_bidi_phase1_result
unicode_bidi_phase1(std::span<char32_t const> code_points, unicode_bidi_class mode)
{
    auto embedding_levels = std::vector<int8_t>(code_points.size(), 0);

    auto original_directions = std::vector<unicode_bidi_class>{};
    original_directions.reserve(code_points.size());
    for (auto const code_point : code_points) {
        original_directions.push_back(ucd_get_bidi_class(code_point));
    }
    auto directions = original_directions;

    auto brackets = std::vector<unicode_bidi_paired_bracket_type>{};
    brackets.reserve(code_points.size());
    for (auto const code_point : code_points) {
        brackets.push_back(ucd_get_bidi_paired_bracket_type(code_point));
    }

    unicode_bidi_P1(directions, embedding_levels, brackets, code_points, mode);

    return {std::move(embedding_levels), std::move(original_directions), std::move(directions), std::move(brackets)};
}
}