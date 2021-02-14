#!/usr/bin/env python3

import functools
import itertools
import operator
import pprint
import random
import signal
import sys
import tools
import unittest

import egirl
import jackblack
import jean
import jo_link_noir
import lazor
import linek
import miremi9
import niceguy
import teckel
import titi

N = 1_000

testlists = [
    [],
    [0],
    [1, 2],
    [2, 1],
    [1, 2, 3],
    [3, 2, 1],
    [2, 1, 3],
    [1, 1, 2],
    [1, 2, 1],
    *[[random.randint(-10, 10)
       for _ in range(random.randint(5, 30))]
      for _ in range(40)],
    list(range(-200, 200)),
    list(range(200, -200, -1)),
]

class SortTest(unittest.TestCase):
    def test_funcs(self):
        for func in tools.funcs:
            with self.subTest(func=func.__name__):
                for testlist in testlists:
                    with self.subTest(a=testlist):
                        self.assertEqual(
                            func(testlist.copy(), len(testlist)), sorted(testlist))


@functools.partial(signal.signal, signal.SIGALRM)
def sighandler(a, b):
    raise KeyboardInterrupt()


def perf():

    for N in (100, 1000):
        print(f"\n\nn={N}")

        for name, hugelist in [
            ('random', list(range(N))),
            ('almost sorted', list(itertools.chain.from_iterable([
                list(range(i * N // 10, (i + 1) * N // 10 - 1)) + [-1]
                for i in range(10)
            ]))),
            ('sequence', list(range(N))),
            ('inverse', list(range(N - 1, -1, -1))),
        ]:
            if name == 'random':
                random.shuffle(hugelist)

            print("\n", f"=== {name} ===".center(25))

            for func in tools.funcs:
                if "jackblack" not in func.__name__ and func.__name__ not in ('julien_2', 'julien_3'):
                    continue
                hugecopy = hugelist.copy()
                signal.alarm(10)
                try:
                    func(hugecopy, N)
                except:
                    print("{: >15} DANS LES CHOUX".format(func.__name__))
                else:
                    print("{: >15} {: >9.8f}".format(func.__name__, func.time), flush=True)


if __name__ == '__main__':
    try:
        entrypoint = {'test': unittest.main, 'perf': perf}[sys.argv.pop(1)]
    except (KeyError, IndexError):
        sys.exit("usage: %s %s {test, perf}" % (sys.executable, sys.argv[0]))
    pprint.pprint(tools.funcs)
    entrypoint()

