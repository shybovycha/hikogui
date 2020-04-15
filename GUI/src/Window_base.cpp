// Copyright 2019 Pokitec
// All rights reserved.

#include "TTauri/GUI/Window_base.hpp"
#include "TTauri/GUI/Window.hpp"
#include "TTauri/GUI/Device.hpp"
#include "TTauri/GUI/WindowWidget.hpp"

namespace TTauri::GUI {

using namespace std;

Window_base::Window_base(const std::shared_ptr<WindowDelegate> delegate, const std::string title) :
    state(State::Initializing),
    delegate(move(delegate)),
    title(move(title))
{
}

Window_base::~Window_base()
{
    try {
        gsl_suppress(f.6) {
            if (state != State::NoWindow) {
                LOG_FATAL("Window '{}' was not properly teardown before destruction.", title);
                abort();
            }
            LOG_INFO("Window '{}' has been propertly destructed.", title);
        }
    } catch (...) {
        abort();
    }
}

void Window_base::initialize()
{
    auto lock = std::scoped_lock(guiMutex);

    widget = std::make_unique<Widgets::WindowWidget>(*static_cast<Window *>(this));

    openingWindow();
}

void Window_base::openingWindow() {
    Window *thisWindow = dynamic_cast<Window *>(this);
    assert(thisWindow);
    delegate->openingWindow(*thisWindow);

    auto lock = std::scoped_lock(guiMutex);
    state = State::NoDevice;
    updateToNextKeyboardTarget(nullptr);
}

void Window_base::closingWindow() {
    Window* thisWindow = dynamic_cast<Window*>(this);
    assert(thisWindow);
    delegate->closingWindow(*thisWindow);

    auto lock = std::scoped_lock(guiMutex);
    state = State::NoWindow;
}

void Window_base::setDevice(Device *newDevice)
{
    auto lock = std::scoped_lock(guiMutex);

    if (device) {
        state = State::DeviceLost;
        teardown();
    }

    device = newDevice;
}



}
