cmake_minimum_required(VERSION 3.5)
project(udpgen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp rate_limiter.cpp)
add_executable(udpgen ${SOURCE_FILES})
target_link_libraries(udpgen netsnmp netsnmpagent)
