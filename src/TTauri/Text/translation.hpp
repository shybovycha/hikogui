// Copyright 2020 Pokitec
// All rights reserved.

#pragma once

#include "TTauri/Foundation/expression.hpp"
#include "TTauri/Foundation/hash.hpp"
#include "language.hpp"
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>

namespace tt {

[[nodiscard]] std::string_view get_translation(
    std::string_view context,
    std::string_view msgid,
    long long n=0,
    std::vector<language*> const &languages=language::prefered_languages
) noexcept;

[[nodiscard]] inline std::string_view get_translation(
    std::string_view msgid,
    long long n=0,
    std::vector<language*> const &languages=language::prefered_languages
) noexcept {
    return get_translation("", msgid, n, languages);
}

void add_translation(
    std::string_view context,
    std::string_view msgid,
    language const &language,
    std::vector<std::string> const &plural_forms
) noexcept;

void add_translation(
    std::string_view context,
    std::string_view msgid,
    std::string const &language_code,
    std::vector<std::string> const &plural_forms
) noexcept;


}