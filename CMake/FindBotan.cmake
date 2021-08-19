find_package(PkgConfig)
pkg_check_modules(PC_Botan QUIET botan)

find_path(Botan_INCLUDE_DIR
    NAMES botan/botan.h
    PATHS ${PC_Botan_INCLUDE_DIRS}
    PATH_SUFFIXES botan-2
)

find_library(Botan_LIBRARY
    NAMES botan botan-2 ${PC_Botan_LIBRARIES}
    PATHS ${PC_Botan_LIBRARY_DIRS}
)

set(Botan_VERSION ${PC_Botan_VERSION})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Botan
    FOUND_VAR Botan_FOUND
    REQUIRED_VARS
        Botan_LIBRARY
        Botan_INCLUDE_DIR
    VERSION_VAR Botan_VERSION
)

if(Botan_FOUND)
    set(Botan_LIBRARIES ${Botan_LIBRARY})
    set(Botan_INCLUDE_DIRS ${Botan_INCLUDE_DIR})
    set(Botan_DEFINITIONS ${PC_Botan_CFLAGS_OTHER})

    if(NOT TARGET Botan::Botan)
        add_library(Botan::Botan UNKNOWN IMPORTED)
        set_target_properties(Botan::Botan PROPERTIES
            IMPORTED_LOCATION "${Botan_LIBRARY}"
            INTERFACE_COMPILE_OPTIONS "${PC_Botan_CFLAGS_OTHER}"
            INTERFACE_INCLUDE_DIRECTORIES "${Botan_INCLUDE_DIR}"
        )
    endif()
endif()

mark_as_advanced(
    Botan_INCLUDE_DIR
    Botan_LIBRARY
)
