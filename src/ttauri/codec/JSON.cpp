// Copyright 2019 Pokitec
// All rights reserved.

#include "JSON.hpp"
#include "../tokenizer.hpp"
#include "../strings.hpp"
#include "../datum.hpp"
#include "../exception.hpp"
#include "../error_info.hpp"
#include "../ResourceView.hpp"
#include "../indent.hpp"
#include <vector>
#include <optional>

namespace tt {

struct parse_context_t {
    std::string_view::const_iterator text_begin;

};

[[nodiscard]] static parse_result<datum> parseValue(parse_context_t &context, token_iterator token);

[[nodiscard]] static parse_result<datum> parseArray(parse_context_t &context, token_iterator token)
{
    auto array = datum{datum::vector{}};

    // Required '['
    if ((*token == tokenizer_name_t::Operator) && (*token == "[")) {
        token++; 
    } else {
        return {};
    }

    bool commaAfterValue = true;
    while (true) {
        // A ']' is required at end of configuration-items.
        if ((*token == tokenizer_name_t::Operator) && (*token == "]")) {
            token++;
            break;

        // Required a value.
        } else if (auto result = parseValue(context, token)) {
            if (!commaAfterValue) {
                tt_error_info().set<parse_location_tag>(token->location);
                throw parse_error("Missing expected ','");
            }

            array.push_back(*result);
            token = result.next_token;

            if ((*token == tokenizer_name_t::Operator) && (*token == ",")) {
                token++;
                commaAfterValue = true;
            } else {
                commaAfterValue = false;
            }

        } else {
            tt_error_info().set<parse_location_tag>(token->location);
            throw parse_error("Expecting a value as the next item in an array.");
        }
    }

    return {std::move(array), token};
}

[[nodiscard]] static parse_result<datum> parseObject(parse_context_t &context, token_iterator token)
{
    auto object = datum{datum::map{}};

    // Required '{'
    if ((*token == tokenizer_name_t::Operator) && (*token == "{")) {
        token++; 
    } else {
        return {};
    }

    bool commaAfterValue = true;
    while (true) {
        // A '}' is required at end of configuration-items.
        if ((*token == tokenizer_name_t::Operator) && (*token == "}")) {
            token++;
            break;

        // Required a string name.
        } else if (*token == tokenizer_name_t::StringLiteral) {
            if (!commaAfterValue) {
                tt_error_info().set<parse_location_tag>(token->location);
                throw parse_error("Missing expected ','");
            }

            auto name = static_cast<std::string>(*token++);

            if ((*token == tokenizer_name_t::Operator) && (*token == ":")) {
                token++;
            } else {
                tt_error_info().set<parse_location_tag>(token->location);
                throw parse_error("Missing expected ':'");
            }

            if (auto result = parseValue(context, token)) {
                object[name] = *result;
                token = result.next_token;

            } else {
                tt_error_info().set<parse_location_tag>(token->location);
                throw parse_error("Missing JSON value");
            }

            if ((*token == tokenizer_name_t::Operator) && (*token == ",")) {
                token++;
                commaAfterValue = true;
            } else {
                commaAfterValue = false;
            }

        } else {
            tt_error_info().set<parse_location_tag>(token->location);
            throw parse_error(fmt::format("Unexpected token {}, expected a key or close-brace.", *token));
        }
    }

    return {std::move(object), token};
}

[[nodiscard]] static parse_result<datum> parseValue(parse_context_t &context, token_iterator token)
{
    switch (token->name) {
    case tokenizer_name_t::StringLiteral: {
        auto value = datum{static_cast<std::string>(*token++)};
        return {std::move(value), token};
        } break;
    case tokenizer_name_t::IntegerLiteral: {
        auto value = datum{static_cast<long long>(*token++)};
        return {std::move(value), token};
        } break;
    case tokenizer_name_t::FloatLiteral: {
        auto value = datum{static_cast<double>(*token++)};
        return {std::move(value), token};
        } break;
    case tokenizer_name_t::Name: {
        ttlet name = static_cast<std::string>(*token++);
        if (name == "true") {
            return {datum{true}, token};
        } else if (name == "false") {
            return {datum{false}, token};
        } else if (name == "null") {
            return {datum{datum::null{}}, token};
        } else {
            tt_error_info().set<parse_location_tag>(token->location);
            throw parse_error(fmt::format("Unexpected name '{}'", name));
        }
        } break;
    default:
        if (auto result1 = parseObject(context, token)) {
            return result1;
        } else if (auto result2 = parseArray(context, token)) {
            return result2;
        } else {
            tt_error_info().set<parse_location_tag>(token->location);
            throw parse_error(fmt::format("Unexpected token '{}'", token->name));
        }
    }
}

[[nodiscard]] datum parseJSON(std::string_view text)
{
    token_vector tokens = parseTokens(text);

    datum root;

    tt_axiom(tokens.back() == tokenizer_name_t::End);
    parse_context_t context;
    context.text_begin = text.begin();

    auto token = tokens.begin();

    if (auto result = parseObject(context, token)) {
        root = std::move(*result);
        token = result.next_token;

    } else {
        tt_error_info().set<parse_location_tag>(token->location);
        throw parse_error("Missing JSON object");
    }

    if (*token != tokenizer_name_t::End) {
        tt_error_info().set<parse_location_tag>(token->location);
        throw parse_error("Unexpected text after JSON root object");
    }

    return root;
}

[[nodiscard]] datum parseJSON(URL const &url)
{
    return parseJSON(url.loadView()->string_view());
}

static void dumpJSON_impl(datum const &value, std::string &result, tt::indent indent={})
{
    bool first_item = true;

    switch (value.type()) {
    case datum_type_t::Null:
        result += "null";
        break;

    case datum_type_t::Boolean:
        result += value ? "true" : "false";
        break;

    case datum_type_t::Integer:
        result += tt::to_string(static_cast<long long>(value));
        break;

    case datum_type_t::Float:
        result += tt::to_string(static_cast<double>(value));
        break;

    case datum_type_t::String:
    case datum_type_t::URL:
        result += '"';
        for (ttlet c: static_cast<std::string>(value)) {
            switch (c) {
            case '\n': result += '\\'; result += 'n'; break;
            case '\r': result += '\\'; result += 'r'; break;
            case '\t': result += '\\'; result += 't'; break;
            case '\f': result += '\\'; result += 'f'; break;
            case '"': result += '\\'; result += '"'; break;
            default: result += c;
            }
        }
        result += '"';
        break;

    case datum_type_t::Vector:
        result += indent;
        result += '[';
        result += '\n';

        for (auto i = value.vector_begin(); i != value.vector_end(); i++, first_item = true) {
            if (!first_item) {
                result += ',';
                result += '\n';
            }
            result += indent + 1;

            dumpJSON_impl(*i, result, indent + 1);
        }

        result += '\n';
        result += indent;
        result += ']';
        break;

    case datum_type_t::Map:
        result += indent;
        result += '{';
        result += '\n';

        for (auto i = value.map_begin(); i != value.map_end(); i++, first_item = true) {
            if (!first_item) {
                result += ',';
                result += '\n';
            }
            result += indent + 1;

            dumpJSON_impl(i->first, result, indent + 1);
            result += ':';
            result += ' ';
            dumpJSON_impl(i->second, result, indent + 1);
        }

        result += '\n';
        result += indent;
        result += '}';
        break;

    default:
        tt_no_default();
    }
}

[[nodiscard]] std::string dumpJSON(datum const &root)
{
    auto r = std::string{};
    dumpJSON_impl(root, r);
    r += '\n';
    return r;
}


}
