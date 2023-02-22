// Copyright Take Vos 2021-2022.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

/** @file widgets/momentary_button_widget.hpp Defines momentary_button_widget.
 * @ingroup widgets
 */

#pragma once

#include "abstract_button_widget.hpp"

namespace hi { inline namespace v1 {

/** A momentary button widget.
 * @ingroup widgets
 */
template<fixed_string Name = "">
class momentary_button_widget final : public abstract_button_widget<Name ^ "momentary-button"> {
public:
    using super = abstract_button_widget<Name ^ "momentary-button">;
    using delegate_type = typename super::delegate_type;

    momentary_button_widget(
        widget *parent,
        std::shared_ptr<delegate_type> delegate,
        button_widget_attribute auto&&...attributes) noexcept :
        super(parent, std::move(delegate))
    {
        alignment = alignment::middle_center();
        set_attributes<0>(hi_forward(attributes)...);
    }

    momentary_button_widget(widget *parent, button_widget_attribute auto&&...attributes) noexcept :
        momentary_button_widget(parent, std::make_shared<delegate_type>(), hi_forward(attributes)...)
    {
    }

    /// @privatesection
    [[nodiscard]] box_constraints update_constraints() noexcept override
    {
        _label_constraints = super::update_constraints();

        // On left side a check mark, on right side short-cut. Around the label extra margin.
        hilet extra_size = theme<prefix ^ "spacing", extent2i>{}(this) * 2;

        auto constraints = _label_constraints + extra_size;
        constraints.margins = theme<prefix ^ "margin", marginsi>{}(this);
        return constraints;
    }

    void set_layout(widget_layout const& context) noexcept override
    {
        if (compare_store(layout, context)) {
            hilet inner_margin = theme<prefix ^ "spacing", int>{}(this);
            hilet label_rectangle = aarectanglei{inner_margin, 0, context.width() - inner_margin * 2, context.height()};
            _on_label_shape = _off_label_shape = _other_label_shape =
                box_shape{_label_constraints, label_rectangle, theme<prefix ^ "cap-height", int>{}(this)};
        }
        super::set_layout(context);
    }

    void draw(draw_context const& context) noexcept override
    {
        if (*mode > widget_mode::invisible and overlaps(context, layout)) {
            draw_label_button(context);
            draw_button(context);
        }
    }
    /// @endprivatesection
private:
    box_constraints _label_constraints;

    void draw_label_button(draw_context const& context) noexcept
    {
        // Move the border of the button in the middle of a pixel.
        context.draw_box(
            layout,
            layout.rectangle(),
            theme<prefix ^ "fill.color", color>{}(this),
            theme<prefix ^ "outline.color", color>{}(this),
            theme<prefix ^ "outline.width", int>{}(this),
            border_side::inside,
            theme<prefix ^ "outline.radius", corner_radii>{}(this));
    }
};

}} // namespace hi::v1
