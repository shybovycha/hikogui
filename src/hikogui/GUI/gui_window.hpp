// Copyright Take Vos 2019-2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "gui_window_size.hpp"
#include "mouse_cursor.hpp"
#include "hitbox.hpp"
#include "gui_event.hpp"
#include "keyboard_focus_direction.hpp"
#include "keyboard_focus_group.hpp"
#include "theme.hpp"
#include "widget_intf.hpp"
#include "../unicode/unicode.hpp"
#include "../GFX/GFX.hpp"
#include "../geometry/module.hpp"
#include "../time/module.hpp"
#include "../l10n/l10n.hpp"
#include "../algorithm/module.hpp"
#include "../macros.hpp"
#include <unordered_set>
#include <memory>
#include <mutex>

namespace hi::inline v1 {
class gfx_device;
class gfx_system;
class gfx_surface;
class gui_system;
class keyboard_bindings;

/*! A Window.
 * This Window is backed by a native operating system window with a Vulkan surface.
 * The Window should not have any decorations, which are to be drawn by the GUI, because
 * modern design requires drawing of user interface elements in the border.
 */
class gui_window {
public:
    gui_system& gui;

    std::unique_ptr<gfx_surface> surface;

    /** The current rectangle of the window relative to the screen.
     * The screen rectangle is set by the operating system event loop.
     *
     * This rectangle is used by the operating system event loop hit-testing
     * to determine the position of screen coordinates to window coordinates.
     *
     * The size of this rectangle is used to laying out widgets and setting
     * the size of the gfx_surface during rendering.
     */
    aarectangle rectangle;

    /** The current cursor.
     * Used for optimizing when the operating system cursor is updated.
     * Set to mouse_cursor::None at the start (for the wait icon) and when the
     * operating system is going to display another icon to make sure
     * when it comes back in the application the cursor will be updated
     * correctly.
     */
    mouse_cursor current_mouse_cursor = mouse_cursor::None;

    /*! The window is currently being resized by the user.
     * We can disable expensive redraws during rendering until this
     * is false again.
     */
    bool resizing = false;

    /*! The window is currently active.
     * Widgets may want to reduce redraws, or change colors.
     */
    bool active = false;

    /*! Dots-per-inch of the screen where the window is located.
     * If the window is located on multiple screens then one of the screens is used as
     * the source for the DPI value.
     */
    float dpi = 72.0;

    /** Theme to use to draw the widgets on this window.
     * The sizes and colors of the theme have already been adjusted to the window's state and dpi.
     */
    hi::theme theme = {};

    /** The size of the widget.
     */
    extent2 widget_size;

    /** Notifier used when the window is closing.
     * It is expected that after notifying these callbacks the instance of this class is destroyed.
     */
    notifier<void()> closing;

    gui_window(gui_system& gui, std::unique_ptr<widget_intf> widget) noexcept : gui(gui), _widget(std::move(widget)) {}

    virtual ~gui_window();

    gui_window(gui_window const&) = delete;
    gui_window& operator=(gui_window const&) = delete;
    gui_window(gui_window&&) = delete;
    gui_window& operator=(gui_window&&) = delete;

    void set_device(gfx_device *device) noexcept;

    /** Get the keyboard binding.
     */
    hi::keyboard_bindings const& keyboard_bindings() const noexcept;

    /** Update window.
     * This will update animations and redraw all widgets managed by this window.
     */
    virtual void render(utc_nanoseconds displayTimePoint);

    template<typename Widget>
    [[nodiscard]] Widget& widget() const noexcept
    {
        return up_cast<Widget>(*_widget);
    }

    virtual void set_title(label title) noexcept
    {
        _title = std::move(title);
    }

    /** Set the mouse cursor icon.
     */
    virtual void set_cursor(mouse_cursor cursor) = 0;

    /** Ask the operating system to close this window.
     */
    virtual void close_window() = 0;

    /** Set the size-state of the window.
     *
     * This function is used to change the size of the window to one
     * of the predefined states: normal, minimized, maximized or full-screen.
     */
    virtual void set_size_state(gui_window_size state) noexcept = 0;

    /** The rectangle of the workspace of the screen where the window is currently located.
     */
    virtual aarectangle workspace_rectangle() const noexcept = 0;

    /** The rectangle of the screen where the window is currently located.
     */
    virtual aarectangle fullscreen_rectangle() const noexcept = 0;

    virtual hi::subpixel_orientation subpixel_orientation() const noexcept = 0;

    /** Get the size-state of the window.
     */
    gui_window_size size_state() const noexcept
    {
        return _size_state;
    }

    /** Open the system menu of the window.
     *
     * On windows 10 this is activated by pressing Alt followed by Spacebar.
     */
    virtual void open_system_menu() = 0;

    /** Ask the operating system to set the size of this window.
     */
    virtual void set_window_size(extent2 extent) = 0;

    void update_mouse_target(widget_id new_target_widget, point2 position = {}) noexcept;

    /** Change the keyboard focus to the given widget.
     * If the group of the widget is incorrect then no widget will be in focus.
     *
     * @param widget The new widget to focus, or empty to remove all keyboard focus.
     * @param group The group the widget must belong to.
     */
    void update_keyboard_target(widget_id widget, keyboard_focus_group group = keyboard_focus_group::normal) noexcept;

    /** Change the keyboard focus to the previous or next widget from the given widget.
     * This function will find the closest widget from the given widget which belongs to the given
     * group; if none is found, or if the original selected widget is found, then no widget will be in focus.
     *
     * @param widget The widget to use as the start point for a new widget to select.
     * @param group The group the widget must belong to.
     * @param direction The direction to search in, or current to select the current widget.
     */
    void update_keyboard_target(widget_id widget, keyboard_focus_group group, keyboard_focus_direction direction) noexcept;

    /** Change the keyboard focus to the given, previous or next widget.
     * This function will find the closest widget from the current widget which belongs to the given
     * group; if none is found, or if the original selected widget is found, then no widget will be in focus.
     *
     * @param group The group the widget must belong to.
     * @param direction The direction to search in, or current to select the current widget.
     */
    void update_keyboard_target(keyboard_focus_group group, keyboard_focus_direction direction) noexcept;

    /** Get text from the clipboard.
     *
     * @note This is part of the window as some operating systems need to know from which window the text was posted.
     * @return The text from the clipboard.
     * @retval empty When the clipboard is locked by another application, on error, if the data on the clipboard can not
     *               be converted to text or if the clipboard is empty.
     */
    [[nodiscard]] virtual std::optional<gstring> get_text_from_clipboard() const noexcept = 0;

    /** Put text on the clipboard.
     *
     * @note This is part of the window as some operating systems need to know from which window the text was posted.
     * @param text The text to place on the clipboard.
     */
    virtual void put_text_on_clipboard(gstring_view text) const noexcept = 0;

    [[nodiscard]] translate2 window_to_screen() const noexcept
    {
        return translate2{rectangle.left(), rectangle.bottom()};
    }

    [[nodiscard]] translate2 screen_to_window() const noexcept
    {
        return ~window_to_screen();
    }

    /** Process the event.
     *
     * This is called by the event handler to start processing events.
     * The events are translated and then uses `send_event_to_widget()` to send the
     * events to the widgets in some priority ordering.
     *
     * It may also be called from within the `event_handle()` of widgets.
     */
    bool process_event(gui_event const& event) noexcept;

protected:
    constexpr static std::chrono::nanoseconds _animation_duration = std::chrono::milliseconds(150);

    /** The label of the window that is passed to the operating system.
     */
    label _title;

    /** The widget covering the complete window.
     */
    std::unique_ptr<widget_intf> _widget;

    box_constraints _widget_constraints = {};

    std::atomic<aarectangle> _redraw_rectangle = aarectangle{};
    std::atomic<bool> _relayout = false;
    std::atomic<bool> _reconstrain = false;
    std::atomic<bool> _resize = false;

    /** Current size state of the window.
     */
    gui_window_size _size_state = gui_window_size::normal;

    /** When the window is minimized, maximized or made full-screen the original size is stored here.
     */
    aarectangle _restore_rectangle;

    /** The time of the last forced redraw.
     * A forced redraw may happen when needing to draw outside
     * of the event-loop. For example when win32 moving or resizing
     * the event-loop is stuck, so forced redraws are happening on
     * the WM_MOVING and WM_SIZING messages that are generated outside
     * the event loop, but on the same thread as the event loop.
     */
    utc_nanoseconds last_forced_redraw = {};

    /** The animated version of the `active` flag.
     */
    animator<float> _animated_active = _animation_duration;

    /** Let the operating system create the actual window.
     * @pre title and extent must be set.
     */
    virtual void create_window(extent2 new_size) = 0;

private:
    /** Target of the mouse
     * Since any mouse event will change the target this is used
     * to check if the target has changed, to send exit events to the previous mouse target.
     */
    widget_id _mouse_target_id;

    /** Target of the keyboard
     * widget where keyboard events are sent to.
     */
    widget_id _keyboard_target_id;

    /** Send event to a target widget.
     *
     * The commands are send in order, until the command is handled, then processing stops immediately.
     * All commands are tried in a batch to the following handlers:
     *  - The target widget
     *  - The parents of the widget up to and including the root widget.
     *  - The window itself.
     */
    bool send_events_to_widget(widget_id target_widget, std::vector<gui_event> const& events) noexcept;

    friend class widget;
};

} // namespace hi::inline v1
