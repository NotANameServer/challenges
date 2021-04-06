cmake_minimum_required(VERSION 3.16)
project(http_counter)

set(CMAKE_CXX_STANDARD 17)

add_executable(http_counter main.cpp HttpServer.cpp HttpServer.h Compteur.cpp Compteur.h Request.cpp Request.h Response.cpp Response.h utils.h http_status_codes.h jsoncpp.cpp json/json.h json/json-forwards.h)