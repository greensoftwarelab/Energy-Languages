message(STATUS "Checking RAPL support")

set(RAPL_MSR_PKG_SUPPORTED FALSE)
set(RAPL_MSR_PP0_SUPPORTED FALSE)
set(RAPL_MSR_PP1_SUPPORTED FALSE)
set(RAPL_MSR_DRAM_SUPPORTED FALSE)

try_run(
    RAPL_MSR_OPEN_RUN_RESULT
    RAPL_MSR_OPEN_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/check_msr_open.cpp
)

if(NOT RAPL_MSR_OPEN_COMPILE_RESULT OR RAPL_MSR_OPEN_RUN_RESULT)
    message(WARNING "Could not open /dev/cpu/0/msr. \
        Did you configure with sudo? \
        If so, make sure your CPU supports RAPL. \
        Configuration will continue and building the project will work, but will crash at runtime.")
    # Set everything to true to test compilation.
    set(RAPL_MSR_PKG_SUPPORTED TRUE)
    set(RAPL_MSR_PP0_SUPPORTED TRUE)
    set(RAPL_MSR_PP1_SUPPORTED TRUE)
    set(RAPL_MSR_DRAM_SUPPORTED TRUE)
    return()
endif()

try_run(
    RAPL_MSR_POWER_UNITS_RUN_RESULT
    RAPL_MSR_POWER_UNITS_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/check_msr_power_units.cpp
)

if(NOT RAPL_MSR_POWER_UNITS_COMPILE_RESULT OR RAPL_MSR_POWER_UNITS_RUN_RESULT)
    message(WARNING "Could not read power units from RAPL. \
        This feature is essential to power measurements. \
        Configuration will continue and building the project will work, but will crash at runtime.")
    # Set everything to true to test compilation.
    set(RAPL_MSR_PKG_SUPPORTED TRUE)
    set(RAPL_MSR_PP0_SUPPORTED TRUE)
    set(RAPL_MSR_PP1_SUPPORTED TRUE)
    set(RAPL_MSR_DRAM_SUPPORTED TRUE)
    return()
endif()

try_run(
    RAPL_MSR_PKG_RUN_RESULT
    RAPL_MSR_PKG_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/check_msr_pkg.cpp
)

if(RAPL_MSR_PKG_COMPILE_RESULT AND NOT RAPL_MSR_PKG_RUN_RESULT)
    message(STATUS "RAPL: PKG supported")
    set(RAPL_MSR_PKG_SUPPORTED TRUE)
endif()

try_run(
    RAPL_MSR_PP0_RUN_RESULT
    RAPL_MSR_PP0_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/check_msr_pp0.cpp
)

if(RAPL_MSR_PP0_COMPILE_RESULT AND NOT RAPL_MSR_PP0_RUN_RESULT)
message(STATUS "RAPL: PP0 supported")
    set(RAPL_MSR_PP0_SUPPORTED TRUE)
endif()

try_run(
    RAPL_MSR_PP1_RUN_RESULT
    RAPL_MSR_PP1_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/check_msr_pp1.cpp
)

if(RAPL_MSR_PP1_COMPILE_RESULT AND NOT RAPL_MSR_PP1_RUN_RESULT)
message(STATUS "RAPL: PP1 supported")
    set(RAPL_MSR_PP1_SUPPORTED TRUE)
endif()

try_run(
    RAPL_MSR_DRAM_RUN_RESULT
    RAPL_MSR_DRAM_COMPILE_RESULT
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/check_msr_dram.cpp
)

if(RAPL_MSR_DRAM_COMPILE_RESULT AND NOT RAPL_MSR_DRAM_RUN_RESULT)
message(STATUS "RAPL: DRAM supported")
    set(RAPL_MSR_DRAM_SUPPORTED TRUE)
endif()
