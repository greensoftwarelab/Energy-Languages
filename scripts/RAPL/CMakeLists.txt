cmake_minimum_required(VERSION 3.19)

include(FetchContent)
include(CheckIPOSupported)

project(RAPL)

set(CMAKE_CXX_STANDARD 20)

find_package(Threads REQUIRED)

FetchContent_Declare(
    glaze
    GIT_REPOSITORY https://github.com/stephenberry/glaze.git
    GIT_TAG c01192b984644ae9f24608ee4a3c25fb3fbfdc2d # v1.3.2
)

message(STATUS "Downloading and configuring Glaze")
FetchContent_MakeAvailable(glaze)

add_library(rapl_lib STATIC src/cpu.cpp src/msr.cpp src/rapl.cpp)
target_compile_options(rapl_lib PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_include_directories(rapl_lib PUBLIC include)
target_link_libraries(rapl_lib PRIVATE m)

include(${CMAKE_SOURCE_DIR}/cmake/checkRaplSupport.cmake)
if(RAPL_MSR_PKG_SUPPORTED)
    target_compile_options(rapl_lib PUBLIC -DRAPL_MSR_PKG_SUPPORTED)
endif()
if(RAPL_MSR_PP0_SUPPORTED)
    target_compile_options(rapl_lib PUBLIC -DRAPL_MSR_PP0_SUPPORTED)
endif()
if(RAPL_MSR_PP1_SUPPORTED)
    target_compile_options(rapl_lib PUBLIC -DRAPL_MSR_PP1_SUPPORTED)
endif()
if(RAPL_MSR_DRAM_SUPPORTED)
    target_compile_options(rapl_lib PUBLIC -DRAPL_MSR_DRAM_SUPPORTED)
endif()

add_executable(rapl src/main.cpp)
target_compile_options(rapl PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_link_libraries(rapl PRIVATE rapl_lib)
target_link_libraries(rapl PRIVATE glaze::glaze)
target_link_libraries(rapl PRIVATE Threads::Threads)

add_executable(idle src/idle.cpp)
target_compile_options(idle PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_link_libraries(idle PRIVATE rapl_lib)
target_link_libraries(idle PRIVATE Threads::Threads)

check_ipo_supported(RESULT IPO_SUPPORTED)
if(IPO_SUPPORTED)
    message(DEBUG "Enabling IPO/LTO")
    set_property(TARGET rapl_lib PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_property(TARGET rapl PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_property(TARGET idle PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
else()
    message(DEBUG "IPO/LTO is not supported")
endif()
