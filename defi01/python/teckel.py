from typing import List
from tools import register

def partition_1(a: List[int], l: int, h: int) -> int:
    pivot = a[h]
    i = l - 1

    for j in range(l, h):
        if a[j] < pivot:
            i += 1
            a[i], a[j] = a[j], a[i]
    a[i+1], a[h] = a[h], a[i+1]

    return i + 1

def qs_1(a: List[int], l: int, h: int) -> None:
    if l >= h:
        return
    p = partition_1(a, l, h)

    qs_1(a, l, p - 1)
    qs_1(a, p + 1, h)

@register
def teckel_1(a: List[int], n: int) -> List[int]:
    qs_1(a, 0, n - 1)
    return a




def partition_2(a: List[int], l: int, h: int) -> int:
    pivot = a[(l + h) // 2]
    i, j = l-1, h+1
    while True:
        while True:
            j -= 1
            if a[j] <= pivot:
                break
        while True:
            i += 1
            if a[i] >= pivot:
                break
        if i < j:
            a[i], a[j] = a[j], a[i]
        else:
            return j

def qs_2(a: List[int], l: int, h: int) -> None:
    if l < h:
        p = partition_2(a, l, h)
        qs_2(a, l, p)
        qs_2(a, p + 1, h)


@register
def teckel_2(a: List[int], n: int) -> List[int]:
    qs_2(a, 0, n - 1)
    return a

def median_of_three_3(a, b, c):
    if a <= b and b <= c:
        return b
    if c <= b and b <= a:
        return b
    if a <= c and c <= b:
        return c
    if b <= c and c <= a:
        return c
    return a

def partition_3(a: List[int], l: int, h: int) -> int:
    pivot = median_of_three_3(a[l], a[(l+h-1)//2], a[h-1])
    i, j = l-1, h+1
    while True:
        while True:
            j -= 1
            if a[j] <= pivot:
                break
        while True:
            i += 1
            if a[i] >= pivot:
                break
        if i < j:
            a[i], a[j] = a[j], a[i]
        else:
            return j

def qs_3(a: List[int], l: int, h: int) -> None:
    if l < h:
        p = partition_3(a, l, h)
        qs_3(a, l, p)
        qs_3(a, p + 1, h)


@register
def teckel_3(a: List[int], n: int) -> List[int]:
    qs_3(a, 0, n - 1)
    return a


def partition_4(arr: List[int], pivot: int) -> Tuple[List[int], List[int], List[int]]:
    l, m, h = [], [], []
    for x in arr:
        if x < pivot: 
            l.append(x)
        if x == pivot: 
            m.append(x)
        if x > pivot: 
            h.append(x)
    return l, m, h

@register
def teckel_4(arr: List[int], n: int) -> List[int]:
    if len(arr) <= 1: 
        return arr
    
    pivot = arr[len(arr)//2]

    l, m, h = partition_4(arr, pivot)
    return teckel_4(l, len(arr)) + m + teckel_4(h, len(arr))
