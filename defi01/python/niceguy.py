from tools import register

def sort(a, n):
    nouveau_n = n-1
    for i in range(n):
        for j in range(nouveau_n):
            if a[j] > a[nouveau_n]:
                a[j], a[nouveau_n] = a[nouveau_n], a[j]
        nouveau_n -= 1
    return a


@register
def niceguy_1(a, n):
    return sort(a, n)
