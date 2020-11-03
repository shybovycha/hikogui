// Copyright 2020 Pokitec
// All rights reserved.

#pragma once

#include "widget.hpp"

namespace tt {

class abstract_container_widget : public widget {
public:
    abstract_container_widget(Window &window, std::shared_ptr<widget> parent) noexcept : widget(window, parent)
    {
        if (parent) {
            // Most containers will not draw itself, only its children.
            ttlet lock = std::scoped_lock(GUISystem_mutex);
            _semantic_layer = parent->semantic_layer();
        }
        _margin = 0.0f;
    }

    ~abstract_container_widget() {}

    /** Remove and deallocate all child widgets.
     */
    void clear() noexcept
    {
        _children.clear();
        _request_reconstrain = true;
    }

    /** Add a widget directly to this widget.
     * Thread safety: locks.
     */
    std::shared_ptr<widget> add_widget(std::shared_ptr<widget> widget) noexcept
    {
        ttlet lock = std::scoped_lock(GUISystem_mutex);

        tt_assume(widget->parent.lock().get() == this);
        _children.push_back(widget);
        _request_reconstrain = true;
        window.requestLayout = true;
        return widget;
    }

    /** Add a widget directly to this widget.
     */
    template<typename T, typename... Args>
    std::shared_ptr<T> make_widget(Args &&... args)
    {
        auto tmp = std::make_shared<T>(window, shared_from_this(), std::forward<Args>(args)...);
        tmp->initialize();
        return std::static_pointer_cast<T>(add_widget(std::move(tmp)));
    }

    [[nodiscard]] bool update_constraints() noexcept
    {
        tt_assume(GUISystem_mutex.recurse_lock_count());

        auto has_constrainted = widget::update_constraints();

        for (auto &&child : _children) {
            tt_assume(child->parent.lock() == shared_from_this());
            has_constrainted |= child->update_constraints();
        }

        return has_constrainted;
    }

    bool update_layout(hires_utc_clock::time_point display_time_point, bool need_layout) noexcept
    {
        tt_assume(GUISystem_mutex.recurse_lock_count());

        auto need_redraw = need_layout |= std::exchange(_request_relayout, false);
        for (auto &&child : _children) {
            tt_assume(child->parent.lock().get() == this);
            need_redraw |= child->update_layout(display_time_point, need_layout);
        }

        return widget::update_layout(display_time_point, need_layout) || need_redraw;
    }

    void draw(DrawContext context, hires_utc_clock::time_point display_time_point) noexcept
    {
        tt_assume(GUISystem_mutex.recurse_lock_count());

        for (auto &child : _children) {
            tt_assume(child->parent.lock().get() == this);
            child->draw(child->make_draw_context(context), display_time_point);
        }

        widget::draw(std::move(context), display_time_point);
    }

    HitBox hitbox_test(vec window_position) const noexcept
    {
        ttlet lock = std::scoped_lock(GUISystem_mutex);

        auto r = HitBox{};
        for (ttlet &child : _children) {
            tt_assume(child->parent.lock().get() == this);
            r = std::max(r, child->hitbox_test(window_position));
        }
        return r;
    }

    std::shared_ptr<widget> next_keyboard_widget(
        std::shared_ptr<widget> const &current_keyboard_widget,
        bool reverse) const noexcept
    {
        ttlet lock = std::scoped_lock(GUISystem_mutex);

        // If current_keyboard_widget is empty, then we need to find the first widget that accepts focus.
        auto found = !current_keyboard_widget;

        // The container widget itself accepts focus.
        if (found && !reverse && accepts_focus()) {
            return std::const_pointer_cast<widget>(shared_from_this());
        }

        ssize_t first = reverse ? ssize(_children) - 1 : 0;
        ssize_t last = reverse ? -1 : ssize(_children);
        ssize_t step = reverse ? -1 : 1;
        for (ssize_t i = first; i != last; i += step) {
            auto &&child = _children[i];

            if (found) {
                // Find the first focus accepting widget.
                if (auto tmp = child->next_keyboard_widget({}, reverse)) {
                    return tmp;
                }

            } else if (child == current_keyboard_widget) {
                found = true;

            } else {
                auto tmp = child->next_keyboard_widget(current_keyboard_widget, reverse);
                if (tmp == current_keyboard_widget) {
                    // The current widget was found, but no next widget available in the child.
                    found = true;

                } else if (tmp) {
                    return tmp;
                }
            }
        }

        // The container widget itself accepts focus.
        if (found && reverse && accepts_focus()) {
            return std::const_pointer_cast<widget>(shared_from_this());
        }

        return found ? current_keyboard_widget : std::shared_ptr<widget>{};
    }

protected:
    std::vector<std::shared_ptr<widget>> _children;
};

} // namespace tt