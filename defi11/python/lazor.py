#!/usr/bin/python3

import random


MIN = 1
MAX = 1_000_000
MAXSTEPS = 60


def verify(guess):
    verify.stepcount += 1
    if verify.stepcount > MAXSTEPS:
        raise ValueError(f"Couldn't find {solution} within {MAXSTEPS} steps")

    if solution == guess:
        return 0
    elif solution < guess:
        return -1
    elif solution > guess:
        return 1


def guess():
    lo = MIN
    hi = MAX

    for step in range(50):
        mi = (lo + hi) // 2
        answer = verify(mi)
        if answer == 0:
            return mi
        elif answer == -1:
            hi = mi - 1
        elif answer == 1:
            lo = mi + 1


def guess_rec(lo=MIN, hi=MAX):
    mi = (lo + hi) // 2
    answer = verify(mi)
    if answer == 0:
        return mi
    elif answer == -1:
        return guess_rec(lo, mi - 1)
    else:
        return guess_rec(mi + 1, hi)


for solution in [
        MIN,
        MIN + 1,
        MAX // 10,
        (MIN + MAX) // 2 - 1,
        (MIN + MAX) // 2,
        (MIN + MAX) // 2 + 1,
        MAX - 1,
        MAX,
    ]:
    verify.stepcount = 0
    result = guess()
    assert result == solution, (result, solution)

    verify.stepcount = 0
    result = guess_rec()
    assert result == solution, (result, solution)
