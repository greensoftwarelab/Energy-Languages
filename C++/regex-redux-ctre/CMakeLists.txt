cmake_minimum_required(VERSION 3.16)

project(regex-redux)

include(FetchContent)
include(CheckIPOSupported)

set(CMAKE_CXX_STANDARD 20)

find_package(OpenMP REQUIRED)

FetchContent_Declare(
  ctre
  GIT_REPOSITORY https://github.com/hanickadot/compile-time-regular-expressions.git
  GIT_TAG c095ae765eff0cb002258c8334a66b286ca0bea8 # v3.7.2
)

message(STATUS "Downloading and configuring CTRE")
FetchContent_MakeAvailable(ctre)

add_executable(regexredux_run regexredux.cpp)
target_compile_options(regexredux_run PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_link_libraries(regexredux_run PRIVATE ctre)
target_link_libraries(regexredux_run PRIVATE OpenMP::OpenMP_CXX)

check_ipo_supported(RESULT IPO_SUPPORTED)
if(IPO_SUPPORTED)
    message(STATUS "IPO/LTO supported")
    set_property(TARGET regexredux_run PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
else()
    message(STATUS "IPO/LTO not supported")
endif()
