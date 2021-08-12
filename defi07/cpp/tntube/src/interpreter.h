//
// Created by TNtube on 05/07/2021.
//

#ifndef MATHINTERPRETER_INTERPRETER_H
#define MATHINTERPRETER_INTERPRETER_H

#include "nodes.h"
#include <memory>


class Interpreter {
    private:
        static double add(std::unique_ptr<Node> & node);
        static double sub(std::unique_ptr<Node> & node);
        static double mul(std::unique_ptr<Node> & node);
        static double div(std::unique_ptr<Node> & node);
        static double plus(std::unique_ptr<Node> & node);
        static double minus(std::unique_ptr<Node> & node);
        static double pow(std::unique_ptr<Node> & node);
        static double fact(std::unique_ptr<Node> & node);

    public:
        static double eval(std::unique_ptr<Node> & node);
};


#endif //MATHINTERPRETER_INTERPRETER_H
