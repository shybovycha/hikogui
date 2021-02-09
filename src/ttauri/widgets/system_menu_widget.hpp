// Copyright Take Vos 2020.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "widget.hpp"
#include "../GUI/PipelineImage_Image.hpp"
#include "../Path.hpp"
#include "../icon.hpp"
#include "../stencils/image_stencil.hpp"
#include <memory>
#include <string>
#include <array>


namespace tt {

class system_menu_widget final : public widget {
public:
    using super = widget;

    system_menu_widget(gui_window &window, std::shared_ptr<abstract_container_widget> parent, icon const &icon) noexcept;
    ~system_menu_widget() {}

    [[nodiscard]] bool
    update_constraints(hires_utc_clock::time_point display_time_point, bool need_reconstrain) noexcept override;
    [[nodiscard]] void update_layout(hires_utc_clock::time_point display_time_point, bool need_layout) noexcept override;

    void draw(draw_context context, hires_utc_clock::time_point display_time_point) noexcept override;

    [[nodiscard]] HitBox hitbox_test(f32x4 window_position) const noexcept override;

private:
    std::unique_ptr<image_stencil> _icon_stencil;

    aarect system_menu_rectangle;
};

}