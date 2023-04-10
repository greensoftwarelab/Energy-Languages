cmake_minimum_required(VERSION 3.19)

include(FetchContent)
include(CheckIPOSupported)

project(RAPL)

set(CMAKE_CXX_STANDARD 20)

find_package(Threads REQUIRED)

FetchContent_Declare(
    glaze
    GIT_REPOSITORY https://github.com/stephenberry/glaze.git
    GIT_TAG cf3d92042ecd375b0f08db78d16272a87cb168f0 # v1.1.2
)

message(STATUS "Downloading and configuring Glaze")
FetchContent_MakeAvailable(glaze)

add_executable(rapl src/rapl.cpp src/cpu.cpp src/msr.cpp)
target_compile_options(rapl PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_include_directories(rapl PRIVATE include)
target_link_libraries(rapl PRIVATE glaze::glaze)
target_link_libraries(rapl PRIVATE m)
target_link_libraries(rapl PRIVATE Threads::Threads)

include(${CMAKE_SOURCE_DIR}/cmake/checkRaplSupport.cmake)
if(RAPL_MSR_PKG_SUPPORTED)
    target_compile_options(rapl PUBLIC -DRAPL_MSR_PKG_SUPPORTED)
endif()
if(RAPL_MSR_PP0_SUPPORTED)
    target_compile_options(rapl PUBLIC -DRAPL_MSR_PP0_SUPPORTED)
endif()
if(RAPL_MSR_PP1_SUPPORTED)
    target_compile_options(rapl PUBLIC -DRAPL_MSR_PP1_SUPPORTED)
endif()
if(RAPL_MSR_DRAM_SUPPORTED)
    target_compile_options(rapl PUBLIC -DRAPL_MSR_DRAM_SUPPORTED)
endif()
if(RAPL_MSR_PSYS_SUPPORTED)
    target_compile_options(rapl PUBLIC -DRAPL_MSR_PSYS_SUPPORTED)
endif()

add_executable(rapl-read src/rapl-read.c)
target_link_libraries(rapl-read PRIVATE m)

check_ipo_supported(RESULT IPO_SUPPORTED)
if(IPO_SUPPORTED)
    message(DEBUG "Enabling IPO/LTO")
    set_property(TARGET rapl PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_property(TARGET rapl-read PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
else()
    message(DEBUG "IPO/LTO is not supported")
endif()
