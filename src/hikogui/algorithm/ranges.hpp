// Copyright Take Vos 2021-2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "../utility/utility.hpp"
#include "../macros.hpp"
#include <ranges>
#include <algorithm>
#include <concepts>
#include <type_traits>
#include <vector>
#include <exception>
#include <stdexcept>

hi_export_module(hikogui.algorithm.ranges);

hi_export namespace hi::inline v1 {

template<typename Value, typename Range>
[[nodiscard]] constexpr Value get_first(Range &&range)
{
    auto it = std::ranges::begin(range);
    auto last = std::ranges::end(range);

    if (it == last) {
        throw std::out_of_range{"Range is empty"};
    }

    auto value = *it++;
    return Value{value};
}

template<typename Range>
[[nodiscard]] constexpr Range::value_type get_first(Range&& range)
{
    return get_first<typename Range::value_type>(std::forward<Range>(range));
}

} // namespace hi::inline v1
