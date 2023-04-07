cmake_minimum_required(VERSION 3.19)

include(CheckIPOSupported)

project(RAPL)

set(CMAKE_CXX_STANDARD 20)

add_executable(rapl src/rapl.cpp)
target_include_directories(rapl PRIVATE include)

target_compile_options(rapl PRIVATE -Wall -Wextra -Wpedantic -Werror)

check_ipo_supported(RESULT IPO_SUPPORTED)
if(IPO_SUPPORTED)
    message(DEBUG "Enabling IPO/LTO")
    set_property(TARGET rapl PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
else()
    message(DEBUG "IPO/LTO is not supported")
endif()
