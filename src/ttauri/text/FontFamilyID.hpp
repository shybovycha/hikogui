// Copyright 2020 Pokitec
// All rights reserved.

#pragma once

#include "ttauri/foundation/tagged_id.hpp"

namespace tt {

struct font_family_id_tag {};

using FontFamilyID = tagged_id<uint16_t, font_family_id_tag>;

}