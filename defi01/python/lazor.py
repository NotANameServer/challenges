import asyncio
from functools import partial, wraps
from itertools import takewhile, count, permutations, chain
from math import ceil, floor
from operator import gt, le, methodcaller
from threading import Thread, Event
from time import time, sleep as sleep_
from typing import List, Any

from heap import Heap
from tools import register


async def awaitsort(a, n):
    if not a:
        return a

    b = []
    m = min(a)

    async def yolo(x):
        for _ in range(x - m):
            await asyncio.sleep(0)
        b.append(x)

    await asyncio.wait([yolo(x) for x in a])

    return b

def julien_0(a, n):
    loop = asyncio.new_event_loop()
    b = loop.run_until_complete(awaitsort(a, n))
    loop.close()
    return b



@register
def julien_1(a: List[Any], n: int):
    """Swap each neighbore until they are all sorted"""
    for i in range(n):
        for j in range(n - i - 1):
            if a[j] > a[j + 1]:
                a[j], a[j + 1] = a[j + 1], a[j]
    return a


@register
def julien_2(a: List[Any], n: int):
    """Move each value to the left until this value is sorted"""
    for i in range(1, n):
        while i and a[i - 1] > a[i]:
            a[i], a[i - 1] = a[i - 1], a[i]
            i -= 1
    return a


@register
def julien_3(a: List[Any], n: int):
    """Move each value to the left until this value is sorted"""
    for i in range(1, n):
        value = a[i]
        j = i
        while j > 0 and a[j - 1] > value:
            a[j] = a[j - 1]
            j -= 1
        a[j] = value
    return a


@register
def julien_4(a: List[Any], n: int):
    """Find smallest value and swap it with the i-th value"""
    for i in range(n):
        smallest = i
        for j in range(i + 1, n):
            if a[j] < a[smallest]:
                smallest = j
        a[i], a[smallest] = a[smallest], a[i]
    return a


@register
def julien_5(a: List[Any], n: int):
    """Sequentially heapify the list and pop the largest element"""
    h = Heap(a)
    h.sort()
    return h


def shell(a: List[Any], n: int, gaps: List[int]):
    """:found on internet:"""
    for gap in gaps:
        for i in range(gap, n):
            tmp = a[i]
            while i >= gap and a[i - gap] > tmp:
                a[i] = a[i - gap]
                i -= gap
            a[i] = tmp
    return a


@register
def julien_6(a: List[Any], n: int):
    seq_func = lambda k: floor(n / 2 ** (k + 1))
    gaps = takewhile(partial(le, 1), map(seq_func, count()))
    return shell(a, n, gaps)


@register
def julien_7(a: List[Any], n: int):
    seq_func = lambda k: ceil((9 * (9/4) ** k - 4) / 5)
    gaps = takewhile(partial(gt, n), map(seq_func, count()))
    return shell(a, n, reversed(list(gaps)))


@register
def julien_8(a: List[Any], n: int):
    if n <= 1:
        return a

    tmp = list()
    def fusion(lower, half, upper):
        aa = a
        i = lower
        j = half
        while i < half and j < upper:
            if aa[i] < aa[j]:
                tmp.append(aa[i])
                i += 1
            else:
                tmp.append(aa[j])
                j += 1
        tmp.extend(aa[i:half])
        tmp.extend(aa[j:upper])
        for i in range(upper - 1, lower - 1, -1):
            aa[i] = tmp.pop()

    half_steps = 1
    steps = half_steps << 1
    while steps < n:
        for lower in range(0, n - steps, steps):
            fusion(lower, lower + half_steps, lower + steps)

        lower += steps
        fusion(lower, lower + half_steps, n)

        half_steps = steps
        steps <<= 1
    fusion(0, half_steps, n)
    return a


@register
def julien_9(a: List[Any], n: int):
    if n <= 1:
        return a
    elif n == 2:
        if a[0] > a[1]:
            a[0], a[1] = a[1], a[0]

    lowers = []
    lowers_cnt = 0
    equals = []
    highers = []
    highers_cnt = 0

    pivot = a[floor(n / 2)]
    for i in range(n):
        if a[i] < pivot:
            lowers.append(a[i])
            lowers_cnt += 1
        elif a[i] > pivot:
            highers.append(a[i])
            highers_cnt += 1
        else:
            equals.append(a[i])

    b = julien_9(lowers, lowers_cnt)
    b.extend(equals)
    b.extend(julien_9(highers, highers_cnt))
    return b


@register
def julien_10(a: List[Any], n: int):
    if n <= 1:
        pass
    elif n == 2:
        if a[0] > a[1]:
            a[0], a[1] = a[1], a[0]
    else:
        half = floor(n / 2)
        a1 = julien_10(a[:half], half)
        a2 = julien_10(a[half:], n - half)

        i = 0
        j = 0
        while i < half and j < (n - half):
            if a1[i] < a2[j]:
                a[i + j] = a1[i]
                i += 1
            else:
                a[i + j] = a2[j]
                j += 1
        while i < half:
            a[i + j] = a1[i]
            i += 1
        while j < (n - half):
            a[i + j] = a2[j]
            j += 1
    return a


@register
def julien_11(a: List[int], n: int):
    """Sort by sorting units then decades then hundreds..."""

    if not a:
        return a

    length = max(map(methodcaller('bit_length'), a))

    # Binary-wise unit is the least significant bit, decades is
    # the second lsb and so on. We use a window to select the
    # current bits (two at a time to be a bit faster) and then
    # shift the &-ed value. The resulted values can either be
    # 00, 01, 10 or 11, we use that value as the array index
    # where to store the number.
    for i in range(0, length + 1, 2):
        units_selector = 0b11 << i              # Move the selector to select the two next bits
        units_values = ([], [], [], [])         # 2 bits => 4 possible values
        for j in range(len(a)):
            value = a[j]
            selection = value & units_selector  # Select the bits
            unit = selection >> i               # Shift the selection to have 00, 01, 10 or 11
            units_values[unit].append(value)    # Append the value to the corresponding slot

        a = list(chain.from_iterable(units_values))  # Merge the lists, values are sorted on all
                                                     # the previous bits

    # In two-complement, values with 1 as most-significant bit
    # are negatives and values with 0 as msb are positives.
    # Due to that, negatives values show after positives one,
    # the following code place the windows on one sign bit to
    # replace negatives values before.
    window = 1 << length + 1
    sign = ([], [])
    for j in range(len(a)):
        sign[(a[j] & window) >> length + 1].append(a[j])
    return list(chain.from_iterable(reversed(sign)))

