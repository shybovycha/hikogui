// Copyright 2019 Pokitec
// All rights reserved.

#include "File.hpp"
#include "strings.hpp"
#include "utils.hpp"
#include "logger.hpp"

namespace TTauri {

File::File(URL const &location, AccessMode accessMode) :
    location(location), accessMode(accessMode)
{
#ifdef WIN32
    auto fileName = location.path_wstring();

    DWORD desiredAccess = 0;
    if (accessMode >= AccessMode::RDONLY) {
        desiredAccess |= GENERIC_READ;
    }
    if (accessMode >= AccessMode::WRONLY) {
        desiredAccess |= GENERIC_WRITE;
    }

    DWORD shareMode;
    if (accessMode >= AccessMode::WRLOCK) {
        shareMode = 0;
    } else if (accessMode >= AccessMode::WRLOCK) {
        shareMode = FILE_SHARE_READ;
    } else {
        shareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
    }

    DWORD creationDisposition;
    if (accessMode >= AccessMode::EXCL) {
        creationDisposition = CREATE_NEW;
    } else if (accessMode >= (AccessMode::CREAT | AccessMode::TRUNC)) {
        creationDisposition = CREATE_ALWAYS;
    } else if (accessMode >= AccessMode::CREAT) {
        creationDisposition = OPEN_ALWAYS;
    } else if (accessMode >= AccessMode::TRUNC) {
        creationDisposition = TRUNCATE_EXISTING;
    } else {
        creationDisposition = OPEN_EXISTING;
    }

    DWORD flagsAndAttributes = 0;
    if (accessMode >= AccessMode::RANDOM_ACCESS) {
        flagsAndAttributes |= FILE_FLAG_RANDOM_ACCESS;
    }
    if (accessMode >= AccessMode::SEQUENTIAL) {
        flagsAndAttributes |= FILE_FLAG_SEQUENTIAL_SCAN;
    }
    if (accessMode >= AccessMode::WRITE_THROUGH) {
        flagsAndAttributes |= FILE_FLAG_WRITE_THROUGH;
    }

    if ((intrinsic = CreateFileW(fileName.data(), desiredAccess, shareMode, NULL, creationDisposition, flagsAndAttributes, NULL)) == INVALID_HANDLE_VALUE) {
        TTAURI_THROW(io_error("Could not open file")
            << error_info<"error_message"_tag>(getLastErrorMessage())
            << error_info<"url"_tag>(location)
        );
    }
#endif
}

File::~File() noexcept
{
    close();
}

void File::close()
{
    if (intrinsic != INVALID_HANDLE_VALUE) {
        if (!CloseHandle(intrinsic)) {
            TTAURI_THROW(io_error("Could not close file")
                << error_info<"error_message"_tag>(getLastErrorMessage())
                << error_info<"url"_tag>(location)
            );
        }
        intrinsic = INVALID_HANDLE_VALUE;
    }
}

}
