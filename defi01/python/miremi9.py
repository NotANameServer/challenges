from tools import register


def tamiser(li, node, n) :
	k = node
	j = 2*k
	while j <= n-1:
		can = True
		try:
			li[j+1]
		except:
			can = False

		if can and j <n and li[j] < li[j+1]:
			j+=1
			
		if li[k] < li[j]:
			li[k],li[j] = li[j],li[k]		
			k = j
			j = 2*k
		else:
			j = n+1


@register
def miremi9(li, length) :
	for i in range(int(length/2),-1,-1):
		tamiser(li, i, length)

	for i in range(length-1,0,-1):
		 li[i],li[0] = li[0],li[i]		
		 tamiser(li,0, i-1)

	return li

