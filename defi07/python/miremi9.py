import re
import sys
nb = "(\-{0,1}[0-9]+(\.[0-9])*)"


def facto(x):
    s = 1
    for k in range(1,x+1):
        s*=k
    return s

solo = {"!":lambda x:facto(int(x))}

first = {"\*\*":lambda x,y:x**y,
			"\^":lambda x,y:x**y
			}
			
second = {"\*":lambda x,y:x*y,
				"\/":lambda x,y:x/y}

third = {"\+":lambda x,y:x+y,
			"\-" :lambda x,y:x-y
}


def calc(si):
	print(si)
	m = re.search("^(\-{0,1}[0-9]+(\.[0-9])*)$", si)    #nombre
	if m:
		return float(m.group(0))
		

	m = re.search("\((.*)\)", si)    #parenthese    
	if m:    
		return calc(si.replace(m.group(0),str(calc(m.group(1)))))


	for key,v in solo.items():
		m = re.search(f'{nb}\s*{key}',si)
		if m: 
			return calc(si.replace(m.group(0),str(v(float(m.group(1))))))

	for key,v in first.items():
		m = re.search(f"({nb})\s*{key}\s*({nb})",si)
		if m:
			return calc(si.replace(m.group(0),str(v(float(m.group(2)),float(m.group(4))))))
	
	
	for key,v in second.items():
		m = re.search(f"({nb})\s*{key}\s*({nb})",si)
		if m:
			return calc(si.replace(m.group(0),str(v(float(m.group(2)),float(m.group(4))))))		
	
	for key,v in third.items():
		m = re.search(f"({nb})\s*{key}\s*({nb})",si)
		if m:
			return calc(si.replace(m.group(0),str(v(float(m.group(2)),float(m.group(4))))))
	

print(calc(" ".join(sys.argv[1:])))
