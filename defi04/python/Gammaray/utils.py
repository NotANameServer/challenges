import os


class Request(object):
	"""
	A Request object have 5 attributes:
		-raw_content: The list of all the headers 
		-method: The method submitted (should be either "GET" or "POST")
		-path: The path requested
		-protocol: The protocol and the version of transmission
		-headers: A dict listing all the headers
	"""
	def __init__(self, raw_request: str):
		self.raw_content = [line.strip() for line in raw_request.split('\n')]
		start = self.raw_content[0].split(' ')

		self.method = start[0]
		if self.method not in METHODS:
			self.http_request = False
			return

		self.protocol = start[2]
		self.http_request = True

		splitted_path = start[1].split('?')
		self.path = splitted_path.pop(0)
		if splitted_path:
			self.variables = splitted_path
		else:
			self.variables = ""

		self.body = self.raw_content[-1]
	
		self.headers = {}
		for i, line in enumerate(self.raw_content[1:]):
			if not line:
				i += 1
				break

			header = line.split(':')
			self.headers[header[0]] = ':'.join(header[1:])

		self.body = '\n'.join(self.raw_content[i+1:])  # to ignore the empty line


class Response(object):
	def __init__(self):
		self.status_code = 200
		self.status_text = "OK"
		self.body = ""

		self.headers = { "Content-Type": "text/html" }

	def create(self):
		startup = f"HTTP/1.0 {self.status_code} {self.status_text}\n"
		headers = ""
		for key, value in self.headers.items():
			headers += f"{key}: {value}\n"

		if type(self.body) == str:
			response = (startup + headers + '\n' + self.body).encode("utf-8")
		else:
			response = (startup + headers + '\n').encode("utf-8") + self.body

		return response



class MethodError(Exception):
	def __init__(self, content=None):
		self.content = content

	def __str__(self):
		return self.content if self.content is not None else "Method not allowed."

class PathNameError(Exception):
	def __init__(self, content=None):
		self.content = content

	def __str__(self):
		return self.content if self.content is not None else "Path name should always starts with \"/\""


def get_static_files():
	try:
		return ("./static", list(map(lambda file: f"/{file}", os.listdir("./static"))))
	except FileNotFoundError:
		return (None, None)

def log(content: str):
	with open("log.txt", "a") as f:
		f.write('\n')
		f.write(content)


def sample_404():
	return """<html>
<body>
<h1>Error 404, page not found.</h1>
</body>
</html>"""


METHODS = ["GET", "POST", "PUT", "DELETE"]
