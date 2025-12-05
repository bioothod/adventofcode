cmake_minimum_required(VERSION 3.15)
project(AdventOfCode2025 CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

add_compile_options(-Wall -Wextra -Wpedantic)
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    add_compile_options(-O3 -march=native)
else()
    add_compile_options(-g -O0 -fsanitize=address -fsanitize=undefined)
    add_link_options(-fsanitize=address -fsanitize=undefined)
endif()

add_library(aoc_common STATIC
    src/common/utils.cpp
)
target_include_directories(aoc_common PUBLIC
    ${CMAKE_SOURCE_DIR}/include
)

# External libraries
find_package(Threads REQUIRED)
target_link_libraries(aoc_common PUBLIC Threads::Threads)

# Optional external libraries
#option(USE_FMT "Use fmt library" OFF)
#if(USE_FMT)
#    find_package(fmt REQUIRED)
#    target_link_libraries(aoc_common PUBLIC fmt::fmt)
#endif()

# Add a day's solution
macro(add_day DAY)
    set(target day${DAY})
    add_executable(${target} src/${DAY}/solution.cpp)
    target_link_libraries(${target} PRIVATE aoc_common)

    if(ARGN)
        target_link_libraries(${target} PRIVATE ${ARGN})
    endif()
endmacro()

# Add all days, target will be `day01`
add_day(01)
add_day(02)
add_day(03)
# add_day(04)
# add_day(05 Boost::regex)
# add_day(06)
# add_day(07)
# add_day(08)
# add_day(09)
# add_day(10)
# add_day(11)
# add_day(12)
