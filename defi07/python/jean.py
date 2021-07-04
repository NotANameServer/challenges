import operator
from math import factorial
from string import whitespace
from sys import stderr

import tkinter


class Token:
    def visit(self, stack):
        raise NotImplementedError


class Int(Token):
    def __init__(self, value):
        self.value = value

    def visit(self, stack):
        stack.append(self.value)

    def __int__(self):
        return self.value


class Fn(Token):
    def __init__(self, fn):
        self.fn = fn

    def visit(self, stack):
        stack[-1] = self.fn(stack[-1])


class BinOp(Token):
    def __init__(self, op, priority):
        self.op = op
        self.priority = priority

    def visit(self, stack):
        y = stack.pop()
        x = stack.pop()
        stack.append(self.op(x, y))


class LPar(Token):
    pass


op = {
    "+": BinOp(operator.add, 0),
    "-": BinOp(operator.sub, 0),
    "*": BinOp(operator.mul, 1),
    "/": BinOp(operator.floordiv, 1),
    "!": Fn(factorial),
    "^": BinOp(operator.pow, 2),
}


def postfix(l):
    """Prends une liste de tokens valide et calcule le résultat
    de l'opération représentée"""
    stack = []

    for tok in l:
        tok.visit(stack)

    res = stack.pop()

    if stack != []:
        raise ValueError("Stack should be empty")

    return int(res)


def tokenize_post(s):
    """Prends une chaîne de caractères représentant un calcul en notation postfix
    et produit la liste de tokens correspondants"""
    l = []
    idx = 0
    length = len(s)

    while idx < length:
        c = s[idx]
        if c.isdigit():
            nb = ""
            while c.isdigit() and idx < length:
                nb += c
                idx += 1
                if idx < length:
                    c = s[idx]
            l.append(Int(int(nb)))
            continue
        if c in whitespace:
            idx += 1
            continue
        if c in op:
            l.append(op[c])
            idx += 1
            continue
        raise ValueError(f"Unexpected {c}")
    return l


def shunting_yard(s):
    """Transforme une chaîne de caractères en une liste de tokens"""
    l = []
    stack = []
    idx = 0
    length = len(s)

    while idx < length:
        c = s[idx]
        if c.isdigit():
            nb = ""
            while c.isdigit() and idx < length:
                nb += c
                idx += 1
                if idx < length:
                    c = s[idx]
            l.append(Int(int(nb)))
            continue
        if c in whitespace:
            idx += 1
            continue
        if c in op:
            f = op[c]
            if isinstance(f, Fn):
                l.append(f)
                idx += 1
                continue
            # f is a Binop
            while (
                stack
                and isinstance(stack[-1], BinOp)
                and stack[-1].priority > f.priority
            ):
                l.append(stack.pop())
            stack.append(f)
            idx += 1
            continue
        if c == "(":
            stack.append(LPar())
            idx += 1
            continue
        if c == ")":
            while not isinstance(stack[-1], LPar):
                l.append(stack.pop())
            stack.pop()
            idx += 1
            continue
        raise ValueError(f"Unexpected {c}")

    for tok in stack:
        if isinstance(tok, LPar):
            raise ValueError(f"Unexpected LPAR, misparenthesized")
        l.append(tok)
    return l


class Main(tkinter.Tk):
    def __init__(self, postfix: bool):
        super().__init__()
        self.title("Calculatrice")

        # Input
        entry = tkinter.Frame(self)
        self.entry = tkinter.StringVar(self)
        tkinter.Entry(entry, textvariable=self.entry).grid(column=0, row=0)
        tkinter.Button(entry, text="calc", command=self.calc).grid(column=1, row=0)
        entry.pack()

        # Result
        res = tkinter.Frame(self)
        self.res = tkinter.StringVar(self)
        tkinter.Label(res, textvariable=self.res).grid(column=0, row=0)
        tkinter.Button(res, text="reset", command=self.reset).grid(column=1, row=0)
        res.pack()

        def add_text(s):
            return lambda *_: print(s, file=stderr) or self.entry.set(
                self.entry.get() + s
            )

        # Keyboard
        keyboard = tkinter.Frame(self)
        numpad = tkinter.Frame(keyboard)
        for i in range(10):
            tkinter.Button(numpad, text=i, command=add_text(str(i))).grid(
                column=i % 5, row=i // 5
            )

        numpad.pack()

        symbols = tkinter.Frame(keyboard)
        i = 0
        for sym in op:
            tkinter.Button(symbols, text=sym, command=add_text(sym)).grid(
                column=i % 3, row=i // 3
            )
            i += 1
        self.lpar = tkinter.Button(symbols, text="(", command=add_text("("))
        self.lpar.grid(column=3, row=0)
        self.rpar = tkinter.Button(symbols, text=")", command=add_text(")"))
        self.rpar.grid(column=3, row=1)
        symbols.pack()

        keyboard.pack()

        # Set mode
        self.mode = not postfix
        self.modevar = tkinter.StringVar(self)
        tkinter.Button(self, textvariable=self.modevar, command=self.set_mode).pack()

        self.set_mode()

        # Bindings
        self.bind("<Return>", self.calc)
        self.bind("<Control-BackSpace>", self.reset)

    def calc(self, *_):
        s = self.entry.get()
        print(s, file=stderr)
        try:
            if self.mode:
                l = tokenize_post(s)
            else:
                l = shunting_yard(s)
            res = str(postfix(l))
        except KeyboardInterrupt as e:
            res = "Erreur dans le calcul"
            print(e, file=stderr)
        self.res.set(res)
        print(res, file=stderr)

    def reset(self, *_):
        self.entry.set("")
        self.res.set("")

    def set_mode(self, *_):
        self.mode = not self.mode
        if self.mode:
            self.modevar.set("Passer en infix")
            self.lpar["state"] = tkinter.DISABLED
            self.rpar["state"] = tkinter.DISABLED
        else:
            self.modevar.set("Passer en postfix")
            self.lpar["state"] = tkinter.NORMAL
            self.rpar["state"] = tkinter.NORMAL


root = Main(True)
root.mainloop()
