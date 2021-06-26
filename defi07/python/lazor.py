"""
Simple infix mathematic expression calculator
Author: Julien Castiaux (github.com/Julien00859)
Licence: MIT
"""

from collections import namedtuple
from math import factorial
from numbers import Number
from operator import add, sub, mul, truediv, mod, pow

Op = namedtuple('Op', ('precedence', 'arity', 'function'))

ops = {
    '+': Op(0, 2, add),
    '-': Op(0, 2, sub),
    '*': Op(1, 2, mul),
    '/': Op(1, 2, truediv),
    '%': Op(1, 2, mod),
    '^': Op(2, 2, pow),
    '!': Op(3, 1, factorial),
}

def tokenize(expr):
    tokens = []

    i = 0
    while i < len(expr):
        if expr[i].isdigit() or expr[i] == '.':
            number = []
            try:
                while expr[i].isdigit():
                    number.append(expr[i])
                    i += 1
                if expr[i] == '.':
                    number.append(expr[i])
                    i += 1
                while expr[i].isdigit():
                    number.append(expr[i])
                    i += 1
                if expr[i] in 'eE':
                    number.append(expr[i])
                    i += 1
                while expr[i].isdigit():
                    number.append(expr[i])
                    i += 1
            except IndexError:
                pass
            tokens.append(float("".join(number)))
        elif expr[i] in ops:
            tokens.append(expr[i])
            i += 1
        elif expr[i] in '()':
            tokens.append(expr[i])
            i += 1
        elif expr[i] == ' ':
            i += 1
        else:
            raise SyntaxError(f"invalid character {expr[i]} at position {i}")

    return tokens


def shuntingyard(infix):
    opstack = []
    postfix = []

    for token in infix:
        if isinstance(token, Number):
            postfix.append(token)
        elif token in ops:
            while (
                opstack and opstack[-1] != "("
                and ops[opstack[-1]].precedence >= ops[token].precedence
            ):
                postfix.append(opstack.pop())
            opstack.append(token)
        elif token == "(":
            opstack.append(token)
        elif token == ")":
            try:
                while opstack[-1] != "(":
                    postfix.append(opstack.pop())
                opstack.pop()
            except IndexError as exc:
                raise SyntaxError("Unmatch parenthesis") from exc
    while opstack:
        postfix.append(opstack.pop())

    return postfix


def compute(postfix):
    stack = []

    for token in postfix:
        if isinstance(token, Number):
            stack.append(token)
        elif len(stack) < ops[token].arity:
            raise SyntaxError("Missing operand")
        else:
            stack.append(ops[token].function(
                *reversed([
                    stack.pop()
                    for _ in range(ops[token].arity
                )])
            ))
    if len(stack) != 1:
        raise SyntaxError("Too many operators")
    return stack.pop()


if __name__ == "__main__":
    from sys import argv
    if "--test" in argv:
        assert compute(shuntingyard(tokenize('15 / 3 + (1 + 3) * 2'))) == 13
    else:
        expr = " ".join(argv[1:])
        infix = tokenize(expr)
        print(infix)
        postfix = shuntingyard(infix)
        print(postfix)
        result = compute(postfix)
        print(result)
