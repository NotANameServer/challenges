//
// Created by TNtube on 03/07/2021.
//

#ifndef MATHINTERPRETER_NODES_H
#define MATHINTERPRETER_NODES_H

#include <string>
#include <memory>
#include <utility>


enum NodeKind {
    N_NUM,
    N_ADD,
    N_SUB,
    N_MUL,
    N_DIV,
    N_PLUS,
    N_MINUS,
    N_POW,
    N_FACT
};

struct Node{
    std::unique_ptr<Node> node1;
    std::unique_ptr<Node> node2;
    NodeKind type;
    std::string value;
    Node(std::unique_ptr<Node>&& left, std::unique_ptr<Node>&& right, NodeKind kind, std::string val):
            node1(std::move(left)), node2(std::move(right)), type(kind), value(std::move(val)) {}

    Node(NodeKind kind, std::string val):
            node1(nullptr), node2(nullptr), type(kind), value(std::move(val)){}

    Node(std::unique_ptr<Node>&& node, NodeKind kind, std::string val):
            node1(std::move(node)), node2(nullptr), type(kind), value(std::move(val)){}
    Node(std::unique_ptr<Node>& node, NodeKind kind, std::string val):
            node1(std::move(node)), node2(nullptr), type(kind), value(std::move(val)){}
};

#endif //MATHINTERPRETER_NODES_H
