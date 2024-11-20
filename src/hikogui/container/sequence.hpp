// Copyright Take Vos 2024.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "../utility/utility.hpp"
#include "../macros.hpp"
#include <type_traits>
#include <compare>
#include <iterator>
#include <ranges>
#include <vector>

hi_export_module(hikogui.algorithm.recursive_iterator);

hi_export namespace hi::inline v1 {

/** A sequence of ranges.
 *
 * @tparam Pair The pair type of the range. Default is std::pair<size_t,
 *         size_t>. The first element is the first index of the range, and the
 *         second element is one beyond the last index of the range.
 */
template<typename Pair = std::pair<size_t, size_t>>
class sequence {
private:
    [[nodiscard]] consteval static auto pair_value_type() const noexcept
    {
        auto const [first, _] = std::declval<Pair>();
        return first;
    }

public:
    using pair_type = Pair;
    using value_type = decltype(pair_value_type());
    using container_type = std::vector<range_type>;

    struct const_iterator {
        sequence const* sequence = nullptr;
        size_t major_index = 0;
        value_type minor_index = 0;

        constexpr const_iterator(const_iterator const&) noexcept = default;
        constexpr const_iterator(const_iterator&&) noexcept = default;
        constexpr const_iterator& operator=(const_iterator const&) noexcept = default;
        constexpr const_iterator& operator=(const_iterator&&) noexcept = default;
        [[nodiscard]] constexpr friend bool operator==(const_iterator const&, const_iterator const&) noexcept = default;
        [[nodiscard]] constexpr friend bool operator<=>(const_iterator const&, const_iterator const&) noexcept = default;

        constexpr const_iterator(
            sequence const* sequence,
            size_t major_index = 0,
            value_type minor_index = 0) noexcept :
            sequence(sequence), major_index(major_index), minor_index(minor_index)
        {
        }

        [[nodiscard]] constexpr value_type operator*() const
        {
            assert(sequence != nullptr);
            auto const [first, last] = sequence->range_at(major_index);
            auto const r = first + minor_index;
            assert(r < last);
            return r;
        }

        constexpr const_iterator& operator++() noexcept
        {
            return advance(1);
        }

        constexpr const_iterator operator++(int) noexcept
        {
            auto r = *this;
            advance(1);
            return r;
        }

        constexpr const_iterator& operator--() noexcept
        {
            return retreat(1);
        }

        constexpr const_iterator operator--(int) noexcept
        {
            auto r = *this;
            retreat(1);
            return r;
        }

        constexpr const_iterator& operator+=(ptrdiff_t n) noexcept
        {
            if (n >= 0) {
                return advance(static_cast<size_t>(n));
            } else {
                return retreat(-static_cast<size_t>(n));
            }
        }

        constexpr const_iterator& operator-=(ptrdiff_t n) noexcept
        {
            if (n >= 0) {
                return retreat(static_cast<size_t>(n));
            } else {
                return advance(-static_cast<size_t>(n));
            }
        }

        [[nodiscard]] constexpr const_iterator operator+(ptrdiff_t n) const noexcept
        {
            auto r = *this;
            r += n;
            return r;
        }

        [[nodiscard]] constexpr const_iterator operator-(ptrdiff_t n) const noexcept
        {
            auto r = *this;
            r -= n;
            return r;
        }

    private:
        constexpr const_iterator& advance(size_t n)
        {
            assert(sequence != nullptr);
            while (n != 0) {
                auto const [first, last] = sequence->range_at(major_index);
                auto const size = last - first;
                auto const remaining = size - minor_index;
                auto const m = std::min(n, remaining);
                minor_index += m;
                n -= m;
                if (minor_index == size) {
                    ++major_index;
                    minor_index = 0;
                }
            }
            return *this;
        }

        constexpr const_iterator& retreat(size_t n)
        {
            assert(sequence != nullptr);
            while (n != 0) {
                if (minor_index == 0) {
                    assert(major_index != 0);
                    --major_index;
                    auto const [first, last] = sequence->range_at(major_index);
                    minor_index = last - first;
                }

                auto const m = std::min(n, minor_index);
                minor_index -= m;
                n -= m;
            }
            return *this;
        }
    };

    constexpr sequence() noexcept = default;
    constexpr sequence(sequence const&) = default;
    constexpr sequence(sequence&&) noexcept(std::is_nothrow_move_constructible_v<container_type>) = default;
    constexpr sequence& operator=(sequence const&) = default;
    constexpr sequence& operator=(sequence&&) noexcept(std::is_nothrow_move_assignable_v<container_type>) = default;

    [[nodiscard]] constexpr const_iterator begin() const noexcept
    {
        return const_iterator{this};
    }

    [[nodiscard]] constexpr const_iterator end() const noexcept
    {
        return const_iterator{this, size()};
    }

    [[nodiscard]] constexpr size_t size() const noexcept
    {
        return std::accumulate(_sequence.begin(), _sequence.end(), size_t{0}, [](size_t a, sequence_range const& b) {
            return a + b.size();
        });
    }

    [[nodiscard]] constexpr bool empty() const noexcept
    {
        return _sequence.empty();
    }

    constexpr void clear() noexcept
    {
        _sequence.clear();
    }

    constexpr void push_back(sequence_range const& range)
    {
        _sequence.push_back(range);
    }

    constexpr void emplace_back(size_t first, size_t last)
    {
        _sequence.emplace_back(first, last);
    }

    constexpr void pop_back()
    {
        _sequence.pop_back();
    }

private:
    container_type _sequence;

    [[nodiscard]] constexpr pair_type const& range_at(size_t i) const
    {
        if (i >= _sequence.size()) {
            throw std::out_of_range{"sequence_range::range_at"};
        }
        return _sequence[i];
    }
};

}
