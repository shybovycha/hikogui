// Copyright 2019 Pokitec
// All rights reserved.

#pragma once

#include "TTauri/Config/ASTExpression.hpp"
#include <string>

namespace TTauri::Config {

struct ASTMember : ASTExpression {
    ASTExpression *object;
    std::string name;

    ASTMember(Location location, ASTExpression *object, char *name) noexcept : ASTExpression(location), object(object), name(name) {
        free(name);
    }

    ~ASTMember() {
        delete object;
    }

    std::string string() const noexcept override {
        return object->string() + "." + name;
    }

    std::vector<std::string> getFQName() override {
        auto r = object->getFQName();
        r.push_back(name);
        return r;
    }

    datum &executeLValue(ExecutionContext &context) const override {
        try {
            return object->executeLValue(context)[name];
        } catch (error &e) {
            e.set<"location"_tag>(location);
            throw;
        }
    }

    datum &executeAssignment(ExecutionContext &context, datum other) const override {
        try {
            auto &lv = object->executeLValue(context)[name];
            lv = std::move(other);
            return lv;
        } catch (error &e) {
            e.set<"location"_tag>(location);
            throw;
        }
    }
};

}