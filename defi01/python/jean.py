from tools import register


def fusion(l1, l2):
    """Renvoie une liste contenant les valeurs de l1 et de l2
    triées par ordre croissant"""
    res = []
    len1 = len(l1)
    len2 = len(l2)

    # positions actuelles
    x = 0
    y = 0
    for i in range(len1 + len2):
        if x >= len1:
            return res + l2[y:]
        if y >= len2:
            return res + l1[x:]

        # l1[x] et l2[y] sont valides
        if l1[x] <= l2[y]:
            res.append(l1[x])
            x += 1
        else:
            res.append(l2[y])
            y += 1

    return res


def fusion_sort(l):
    """Renvoie une version triée de la liste"""
    if len(l) <= 1:
        return l
    mid = len(l)//2
    return fusion(fusion_sort(l[:mid]), fusion_sort(l[mid:]))


@register
def jean_1(a, n):
    return fusion_sort(a)
