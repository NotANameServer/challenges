from tools import register

def _counting_sort_impl_1(a, d):
  t256 = [[] for i in range(256)]
  for x in a:
    t256[(x >> d) & 0xff].append(x)

  if d:
    d -= 8
    tmp = t256
    t256 = (_counting_sort_impl_1(l, d) for l in tmp if l)

  return (x for l in t256 for x in l)

def _counting_sort_1(a):
  # def impl(a,d,min):
  #   return _counting_sort_impl((x for x in a if min <= (x >> d) <= 255), d)
  # return (impl(a, d, 1 if d else 0) for d in range(0, 32, 8))
  return (
    _counting_sort_impl_1((x for x in a if x <= 0xff), 0),
    _counting_sort_impl_1((x for x in a if 1 <= (x >> 8) <= 0xff), 8),
    _counting_sort_impl_1((x for x in a if 1 <= (x >> 16) <= 0xff), 16),
    _counting_sort_impl_1((x for x in a if 1 <= (x >> 24) <= 0xff), 24),
  )

def sort_1(a):
  if not a:
    return []

  neg_a = [-x for x in a if x < 0]
  if neg_a:
    r = [-x for l in _counting_sort_1(neg_a) for x in l]
    r.reverse()
    a = [x for x in a if x >= 0]
    r += (x for l in _counting_sort_1(a) for x in l)
    return r

  return [x for l in _counting_sort_1(a) for x in l]


@register
def jo_link_noir_1(a, b):
    return sort_1(a)



def _counting_sort_impl_2(a, d):
  t256 = [[] for i in range(256)]
  for x in a:
    t256[(x >> d) & 0xff].append(x)

  if d:
    d -= 8
    tmp = t256
    t256 = (_counting_sort_impl_2(l, d) for l in tmp if l)

  return (x for l in t256 for x in l)

def sort_2(a):
  t1 = [[] for i in range(256)]
  t2 = [[] for i in range(256)]
  t3 = [[] for i in range(256)]
  t4 = [[] for i in range(256)]
  tn1 = [[] for i in range(256)]
  tn2 = [[] for i in range(256)]
  tn3 = [[] for i in range(256)]
  tn4 = [[] for i in range(256)]

  for x in a:
    if x < 0:
      x = x + 0x7fffffff
      if x <= 0xff: tn1[x].append(x)
      elif 1 <= (x >> 8) <= 0xff: tn2[(x >> 8) & 0xff].append(x)
      elif 1 <= (x >> 16) <= 0xff: tn3[(x >> 16) & 0xff].append(x)
      elif 1 <= (x >> 24) <= 0xff: tn4[(x >> 24) & 0xff].append(x)
    else:
      if x <= 0xff: t1[x].append(x)
      elif 1 <= (x >> 8) <= 0xff: t2[(x >> 8) & 0xff].append(x)
      elif 1 <= (x >> 16) <= 0xff: t3[(x >> 16) & 0xff].append(x)
      elif 1 <= (x >> 24) <= 0xff: t4[(x >> 24) & 0xff].append(x)

  return [x for l in (
    (x - 0x7fffffff for l in tn4 if l for x in _counting_sort_impl_2(l, 24)),
    (x - 0x7fffffff for l in tn3 if l for x in _counting_sort_impl_2(l, 16)),
    (x - 0x7fffffff for l in tn2 if l for x in _counting_sort_impl_2(l, 8)),
    (x - 0x7fffffff for l in tn1 for x in l),
    (x for l in t1 for x in l),
    (x for l in t2 if l for x in _counting_sort_impl_2(l, 8)),
    (x for l in t3 if l for x in _counting_sort_impl_2(l, 16)),
    (x for l in t4 if l for x in _counting_sort_impl_2(l, 24)),
  ) for x in l]


@register
def jo_link_noir_2(a, n):
    return sort_2(a)

