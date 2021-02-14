from tools import register


@register
def egirl_1(l, n):
    sortedL = []
    for _ in range(n):
        sortedL.append(min(l))
        l.remove(min(l))
    return sortedL
