// Copyright Take Vos 2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

/** @file widgets/text_delegate.hpp Defines delegate_delegate and some default text delegates.
 * @ingroup widget_delegates
 */

#pragma once

#include "widget_delegate.hpp"
#include "../observer/observer.hpp"
#include "../utility/utility.hpp"
#include "../concurrency/concurrency.hpp"
#include "../dispatch/dispatch.hpp"
#include "../GUI/GUI.hpp"
#include "../macros.hpp"
#include "../unicode/unicode.hpp"
#include "../l10n/l10n.hpp"
#include "../macros.hpp"
#include <string>
#include <memory>
#include <functional>

hi_export_module(hikogui.widgets.text_delegate);

hi_export namespace hi { inline namespace v1 {

/** A delegate that controls the state of a text_widget.
 *
 * @ingroup widget_delegates
 */
class text_delegate : public virtual widget_delegate {
public:
    [[nodiscard]] virtual bool empty_text(widget_intf const* sender) const = 0;

    /** Read text as a string of graphemes.
     */
    [[nodiscard]] virtual gstring get_text(widget_intf const* sender) const = 0;

    [[nodiscard]] virtual bool mutable_text() const noexcept
    {
        return false;
    }

    /** Write text from a string of graphemes.
     */
    virtual void set_text(widget_intf const* sender, gstring const& text)
    {
        std::unreachable();
    }

    /** Subscribe a callback for notifying the widget of a data change.
     */
    template<forward_of<void()> Func>
    [[nodiscard]] callback<void()> subscribe(widget_intf const* sender, Func&& func, callback_flags flags = callback_flags::synchronous)
    {
        return _notifier.subscribe(std::forward<Func>(func), flags);
    }

protected:
    notifier<void()> _notifier;
};

/** A default text delegate.
 *
 * @ingroup widget_delegates
 * @tparam T The type of the observer value.
 */
template<typename T>
class default_text_delegate;

/** A default text delegate specialization for `std::string`.
 *
 * @ingroup widget_delegates
 */
template<>
class default_text_delegate<char const *> : public text_delegate {
public:
    using value_type = char const *;

    observer<value_type> value;

    /** Construct a delegate.
     *
     * @param value A value or observer-value used as a representation of the state.
     */
    template<forward_of<observer<value_type>> Value>
    explicit default_text_delegate(Value&& value) : get_text(std::forward<Value>(value))
    {
        _value_cbt = this->value.subscribe([&](auto...) {
            this->_notifier();
        });
    }

    [[nodiscard]] bool empty_text(widget_intf const* sender) const override
    {
        return value->empty();
    }

    [[nodiscard]] gstring get_text(widget_intf const* sender) const override
    {
        return to_gstring(std::string{*value});
    }

private:
    callback<void(value_type)> _value_cbt;
};

/** A default text delegate specialization for `std::string`.
 *
 * @ingroup widget_delegates
 */
template<>
class default_text_delegate<std::string> : public text_delegate {
public:
    using value_type = std::string;

    observer<value_type> value;

    /** Construct a delegate.
     *
     * @param value A value or observer-value used as a representation of the state.
     */
    template<forward_of<observer<value_type>> Value>
    explicit default_text_delegate(Value&& value) : get_text(std::forward<Value>(value))
    {
        _value_cbt = this->value.subscribe([&](auto...) {
            this->_notifier();
        });
    }

    [[nodiscard]] bool empty_text(widget_intf const* sender) const override
    {
        return value->empty();
    }

    [[nodiscard]] gstring get_text(widget_intf const* sender) const override
    {
        return to_gstring(*value);
    }

    [[nodiscard]] bool mutable_text(widget_intf const* sender) const override
    {
        return true;
    }

    void set_text(widget_intf const* sender, gstring const& text) override
    {
        value = to_string(text);
    }

private:
    callback<void(value_type)> _value_cbt;
};

/** A default text delegate specialization for `gstring`.
 *
 * @ingroup widget_delegates
 */
template<>
class default_text_delegate<gstring> : public text_delegate {
public:
    using value_type = gstring;

    observer<value_type> value;

    /** Construct a delegate.
     *
     * @param value A value or observer-value used as a representation of the state.
     */
    template<forward_of<observer<value_type>> Value>
    explicit default_text_delegate(Value&& value) : get_text(std::forward<Value>(value))
    {
        _value_cbt = this->value.subscribe([&](auto...) {
            this->_notifier();
        });
    }

    [[nodiscard]] bool empty_text(widget_intf const* sender) const override
    {
        return value->empty();
    }

    [[nodiscard]] gstring get_text(widget_intf const* sender) const override
    {
        return *value;
    }

    [[nodiscard]] bool mutable_text(widget_intf const* sender) const override
    {
        return true;
    }

    void set_text(widget_intf const* sender, gstring const& text) override
    {
        value = text;
    }

private:
    callback<void(value_type)> _value_cbt;
};

/** A default text delegate specialization for `translate`.
 *
 * @ingroup widget_delegates
 */
template<>
class default_text_delegate<txt> : public text_delegate {
public:
    using value_type = txt;

    observer<value_type> value;

    /** Construct a delegate.
     *
     * @param value A value or observer-value used as a representation of the state.
     */
    template<forward_of<observer<value_type>> Value>
    explicit default_text_delegate(Value&& value) : get_text(std::forward<Value>(value))
    {
        _value_cbt = this->value.subscribe([&](auto...) {
            this->_notifier();
        });
    }

    [[nodiscard]] bool empty_text(widget_intf const* sender) override
    {
        return value->empty();
    }

    [[nodiscard]] gstring get_text(widget_intf const* sender) override
    {
        return value->translate();
    }

private:
    callback<void(value_type)> _value_cbt;
};

template<typename Value>
default_text_delegate(Value&&) -> default_text_delegate<observer_decay_t<Value>>;

}} // namespace hi::v1
