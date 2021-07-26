cmake_minimum_required(VERSION 3.19)
project(MathInterpreter)

set(CMAKE_CXX_STANDARD 17)

add_executable(MathInterpreter src/main.cpp src/tokens.h src/lexer.cpp src/lexer.h src/nodes.h src/parser.cpp src/parser.h src/interpreter.cpp src/interpreter.h)