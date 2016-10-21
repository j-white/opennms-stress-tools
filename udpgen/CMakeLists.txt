cmake_minimum_required(VERSION 3.5)
project(oes)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp rate_limiter.cpp)
add_executable(oes ${SOURCE_FILES})
target_link_libraries(oes netsnmp netsnmpagent)
