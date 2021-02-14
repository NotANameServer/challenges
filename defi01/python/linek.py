from tools import register

@register
def linek_1(a, n):
    BASE = 10
    max_reached = True
    digit = -1
    while max_reached:
        max_reached = False
        digit += 1
        partition = {i: [] for i in range(-BASE, BASE)}
        power = BASE ** digit
        for el in a:
            key = el // power % BASE
            if el < 0:
                key = key - BASE
            partition[key].append(el)
            if el > power:
                max_reached = True
        a = []
        for i in range(-BASE, BASE):
            a.extend(partition[i])
    return a


@register
def linek_2(a, n):
    BASE = max(n // 100, 10)
    max_reached = True
    digit = -1
    while max_reached:
        max_reached = False
        digit += 1
        partition = [[] for i in range(-BASE, BASE)]
        power = BASE ** digit
        for el in a:
            key = el // power % BASE
            if el < 0:
                key = key - BASE
            partition[key].append(el)
            if el > power:
                max_reached = True
        a = []
        for i in range(-BASE, BASE):
            a.extend(partition[i])
    return a

