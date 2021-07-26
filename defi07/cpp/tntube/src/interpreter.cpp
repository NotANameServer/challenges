//
// Created by TNtube on 05/07/2021.
//

#include "interpreter.h"
#include <stdexcept>
#include <cmath>

double Interpreter::add(std::unique_ptr<Node> & node) {
    return eval(node->node1) + eval(node->node2);
}

double Interpreter::sub(std::unique_ptr<Node> & node) {
    return eval(node->node1) - eval(node->node2);
}

double Interpreter::mul(std::unique_ptr<Node> & node) {
    return eval(node->node1) * eval(node->node2);
}

double Interpreter::div(std::unique_ptr<Node> & node) {
    double x = eval(node->node1);
    double y = eval(node->node2);
    if (y == 0) {
        throw std::runtime_error("Math error, can't divide by 0");
    }
    return x / y;
}

double Interpreter::plus(std::unique_ptr<Node> & node) {
    return +eval(node->node1);
}

double Interpreter::minus(std::unique_ptr<Node> & node) {
    return -eval(node->node1);
}

double Interpreter::pow(std::unique_ptr<Node> &node) {
    return std::pow(eval(node->node1), eval(node->node2));
}

double Interpreter::fact(std::unique_ptr<Node> &node) {
    return std::tgamma(eval(node->node1) + 1);
}

double Interpreter::eval(std::unique_ptr<Node> & node) {
    switch (node->type) {
        case N_NUM:
            return std::stod(node->value);
        case N_ADD:
            return add(node);
        case N_SUB:
            return sub(node);
        case N_MUL:
            return mul(node);
        case N_DIV:
            return div(node);
        case N_PLUS:
            return plus(node);
        case N_MINUS:
            return minus(node);
        case N_POW:
            return pow(node);
        case N_FACT:
            return fact(node);
        default:
            return 0;
    }
}

