import collections
import functools
import itertools
import time


funcs = []
def register(func):
    @functools.wraps(func)
    def wrapped(*args, **kwargs):
        before = time.perf_counter()
        ret = func(*args, **kwargs)
        after = time.perf_counter()
        wrapped.time = after - before
        return ret
    funcs.append(wrapped)
    return func  # Don't change the function itself


def issorted(a):
    for i in range(1, len(a)):
        if a[i - 1] > a[i]:
            return False
    return True


def ispermutationof(a, b):
    return collections.Counter(a) == collections.Counter(b)
