cmake_minimum_required(VERSION 3.16)

include(FetchContent)

set(CMAKE_CXX_STANDARD 20)

project(rapl)

FetchContent_Declare(
    papi
    GIT_REPOSITORY https://bitbucket.org/icl/papi.git
    GIT_TAG de96060998cd9fc77396c5e100e52e0ea1cdc3c3 # papi-7-0-0-t
)

message("-- Downloading PAPI")
FetchContent_MakeAvailable(papi)

# Actual code and build scripts are located in the src subdirectory.
set(papi_SOURCE_DIR "${papi_SOURCE_DIR}/src")

message("-- Configuring PAPI")
execute_process(
    COMMAND ./configure --prefix=${papi_BINARY_DIR} --with-components=rapl
    WORKING_DIRECTORY ${papi_SOURCE_DIR}
    COMMAND_ERROR_IS_FATAL ANY
    OUTPUT_QUIET
)

message("-- Building PAPI")
execute_process(
    COMMAND make -j
    WORKING_DIRECTORY ${papi_SOURCE_DIR}
    COMMAND_ERROR_IS_FATAL ANY
    OUTPUT_QUIET ERROR_QUIET
)

message("-- Installing PAPI")
execute_process(
    COMMAND make install
    WORKING_DIRECTORY ${papi_SOURCE_DIR}
    COMMAND_ERROR_IS_FATAL ANY
    OUTPUT_QUIET ERROR_QUIET
)

add_library(papi STATIC IMPORTED)
set_property(TARGET papi PROPERTY IMPORTED_LOCATION "${papi_BINARY_DIR}/lib/libpapi.a")
target_include_directories(papi INTERFACE "${papi_BINARY_DIR}/include")

add_executable(rapl main.cpp)
target_link_libraries(rapl papi)

add_executable(rapl-read rapl-read.c)
target_link_libraries(rapl-read PRIVATE m)
