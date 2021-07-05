//
// Created by TNtube on 03/07/2021.
//

#include "parser.h"
#include <stdexcept>

std::unique_ptr<Node> Parser::parse() {
    auto actual = tokenVec.begin();
    if (actual == tokenVec.end()) {
        error("No expression");
    }

    std::unique_ptr<Node> result = expr(actual);

    if(actual != tokenVec.end()){
        error("Syntax Error");
    }
    return result;
}

std::unique_ptr<Node> Parser::expr(std::vector<Token>::iterator & actual) {
    std::unique_ptr<Node> result = term(actual);

    while (actual != tokenVec.end() && (actual->type == T_ADD || actual->type == T_SUB)){
        auto token = actual;
        if (actual->type == T_ADD) {
            next(actual);
            result = std::make_unique<Node>(std::move(result), term(actual), N_ADD, token->value);
        } else {
            next(actual);
            result = std::make_unique<Node>(std::move(result), term(actual), N_SUB, token->value);
        }
    }
    return result;
}

std::unique_ptr<Node> Parser::term(std::vector<Token>::iterator & actual) {
    std::unique_ptr<Node> result = pow(actual);


    while (actual != tokenVec.end() && (actual->type == T_MUL || actual->type == T_DIV)){
        auto token = actual;

        if (actual->type == T_MUL) {
            next(actual);
            return std::make_unique<Node>(std::move(result), pow(actual), N_MUL, token->value);

        } else {
            next(actual);
            result = std::make_unique<Node>(std::move(result), pow(actual), N_MUL, token->value);
        }
    }
    return result;
}

std::unique_ptr<Node> Parser::pow(std::vector<Token>::iterator & actual) {
    std::unique_ptr<Node> result = factor(actual);


    while (actual != tokenVec.end() && actual->type == T_POW){
        auto token = actual;
        next(actual);
        return std::make_unique<Node>(std::move(result), factor(actual), N_POW, token->value);
    }
    return result;
}


std::unique_ptr<Node> Parser::factor(std::vector<Token>::iterator & actual) {
    auto token = actual;

    if (token->type == TokenID::T_LPAR){
        next(actual);
        auto result = expr(actual);

        if (actual->type != TokenID::T_RPAR){
            error("Missing Closing Parenthesis");
        }
        next(actual);

        if (actual->type == T_FACT) {
            result = std::make_unique<Node>(result, N_FACT, "!");
            next(actual);
        }

        return result;
    }
    else if (token->type == TokenID::T_NUM) {
        next(actual);
        if (actual->type == T_FACT) {
            auto result = std::make_unique<Node>(N_NUM, token->value);
            next(actual);
            return std::make_unique<Node>(result, N_FACT, "!");
        }
        return std::make_unique<Node>(N_NUM, token->value);
    }
    else if (token->type == TokenID::T_ADD) {
        next(actual);
        return std::make_unique<Node>(factor(actual), N_PLUS, "+");
    }
    else if (token->type == TokenID::T_SUB) {
        next(actual);
        return std::make_unique<Node>(factor(actual), N_MINUS, "-");
    }
    throw std::runtime_error("Invalid Syntax");
}

void Parser::next(std::vector<Token>::iterator & actual) {
    actual++;
}

void Parser::error(std::string const & message) {
    throw std::runtime_error(message);
}

Parser::Parser(std::vector<Token> & tokens)
 : tokenVec(tokens)
{}