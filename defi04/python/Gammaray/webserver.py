import threading
import socket

import utils


class WebApp(object):
	def __init__(self, port=8080, ip="0.0.0.0"):
		self.network_info = (ip, port)

		self.paths = {
				'GET': [],
				'POST': [],
				'PUT': [],
				'DELETE': []
				}

		self.request = None
		self.response = None

		self._server = None

	def _get_request(self, client: socket.socket) -> utils.Request:
		"""
		Waits for a connexion from a client and return a
		Request object.
		:param client: The client we are listening to
		:type client: socket.socket
		:return: A request object of the request the client sent
		:rtype: utils.Request
		"""
		req = client.recv(1024).decode("utf-8")

		return utils.Request(req)


	def _handle_client(self, client, keep_log):
		self.request = self._get_request(client)
		self.response = utils.Response()

		if not self.request.http_request:
			client.close()
			return
		
		if keep_log:
			utils.log(f"{':'.join([str(el) for el in client.getpeername()])} - {self.request.raw_content}")

		# If the path exists, we send the corresponding file
		# If not, we send the 404 page
		unknown_path = True
		for paths, webpage in self.paths[self.request.method]:
			pathnames = list(map(lambda el: f"/{el}", paths))
			if self.request.path in pathnames:
				self.response.body = webpage()
				unknown_path = False
				break

		if unknown_path:
			self.response.status_code = 404
			self.response.body = utils.sample_404()

		# Send everthing
		client.send(self.response.create())
		client.close()
		self.request = None
		self.response = None


	def init(self) -> None:
		"""
		Create a network endpoint binded at
		the IP and the PORT of the WebApp and
		open it for connexions.
		"""
		self._server = socket.socket()
		self._server.bind(self.network_info)
		self._server.listen()

		path, static_files = utils.get_static_files()
		if path is not None:
			for filename in static_files:
				self.paths["GET"].append(([filename[1:]], self._send_file(path + filename)))


	def run(self, keep_log=True) -> None:
		"""
		Main loop of the web server
		:param keep_log: Determine either or not the server should keep
		track of the connexions. They are stored in a txt file named "log.txt"
		"""
		print(f"Running web service on http://{self.network_info[0]}:{self.network_info[1]}\nLOG = {keep_log}")
		if keep_log:
			with open("log.txt", 'w') as _:
				pass

		while True:
			client, _ = self._server.accept()
			current_thread = threading.Thread(target=self._handle_client, args=(client, keep_log))
			current_thread.start()


	def load_path(self, pathnames: list, method="GET") -> None:
		"""
		Create a new key in our path dictionnary, and assign
		it the fuction passed as decorator's parameter.
		:param pathname: The relative name of the path "/some_name"
		:param method: The method expected for the path
		"""
		if method not in utils.METHODS:
			raise utils.MethodError()

		def wrapper(func) -> None:
			"""
			Adds the function to the dictionnary
			:param func: the function
			:type func: function
			"""
			self.paths[method].append((pathnames, func))
		return wrapper


	def _send_file(self, pathname: str): # -> function
		def wrapper():
			with open(pathname, "rb") as f:
				return f.read()
		return wrapper
