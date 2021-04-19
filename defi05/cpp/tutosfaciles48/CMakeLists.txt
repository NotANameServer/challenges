cmake_minimum_required(VERSION 3.17)
project(SdlForest)

set(CMAKE_CXX_FLAGS "-Wall")

set(SDL2_IMAGE_INCLUDE_DIR "D:/lib/SDL2-2.0.14/include")
set(SDL2_IMAGE_LIBRARY "D:/lib/SDL2_Image/lib/x64/SDL2_image.lib")

set(SDL2_DIR "D:/lib/SDL2-2.0.14")
set(SDL2_INCLUDE_DIR "D:/lib/SDL2-2.0.14/include")
set(SDL2_LIBRARY "D:/lib/SDL2-2.0.14/lib/x64/SDL2.lib")
set(SDL2_LIB_MAIN "D:/lib/SDL2-2.0.14/lib/x64/SDL2main.lib")

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_MODULE_PATH ${CMAKE_PREFIX_PATH} ${SDL2_DIR}/cmake)

find_package(SDL2 REQUIRED)
find_package(SDL2_image REQUIRED)

add_executable(SdlForest main.cpp Cell.cpp Cell.h util.h)

include_directories(${SDL2_INCLUDE_DIR} ${SDL2_IMAGE_INCLUDE_DIR})
target_link_libraries(SdlForest ${SDL2_LIBRARY} ${SDL2_IMAGE_LIBRARY} ${SDL2_LIB_MAIN})