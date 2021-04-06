from datetime import datetime
import json
import socket
import threading
from concurrent.futures import ThreadPoolExecutor

HTTP_CODES = {
    200: "OK",
    201: "Created",
    204: "No Content",
    400: "Bad Request",
    404: "Not Found",
    414: "Request-URI Too Long",
    500: "Internal Server Error",
}


def make_html(title, body):
    return """\
<!doctype html>
<html lang="fr">
<head>
    <meta charset="utf-8">
    <title>%s</title>
</head>
<body>
%s
</body>
</html>""" % (
        title,
        body,
    )


class Request:
    """ web request """

    def __init__(self, command=b"", datas=b"", headers=None, ip=None):
        self.ip = ip
        self.raw = datas
        self.headers = dict(headers or {})
        self.accepts = self._parse_accept(self.headers.get("Accept", ""))
        self.hostname, *_ = self.headers.get("Host", "").split(":")

        if command:
            self.method, route, self.version = command.decode("ascii").split()
        else:
            self.method = route = self.version = ""
        self.route, self.params = self._parse_route(route)
        content_type, _, charset = self.headers.get("Content-Type", "").partition(";")
        key, _, value = charset.partition("=")
        if content_type == "application/x-www-form-urlencoded":
            self.data = self._parse_url_encoded(self.raw.decode(value or "utf-8"))
        elif content_type == "application/json":
            self.data = json.loads(self.raw.decode(value or "utf-8"))
        else:
            self.data = {}

    def _parse_accept(self, accept):
        res = []
        for leaf in accept.split(","):
            a, *_ = leaf.split(";")
            res.append(a.strip())

        return res

    def _parse_url_encoded(self, datas):
        res = {}
        if datas:
            for param in datas.split("&"):
                key, value = param.split("=")
                res[key] = value
        return res

    def _parse_route(self, route):
        route, dot, params = route.partition("?")
        return route, self._parse_url_encoded(params)

    def _default_headers(self):
        return {
            "Server": "Linek/0.0.0",
            "Date": datetime.utcnow().strftime("%a %d %b %Y %H:%M:%S GMT"),
        }

    def make_response(
        self,
        code=200,
        headers=None,
        datas=None,
    ):
        """ construit la réponse HTTP """
        response_headers = self._default_headers()
        response_headers.update(headers or {})

        if datas:
            response_headers["Content-Length"] = len(datas)
        response = f"HTTP/1.0 {code} {HTTP_CODES[code]}\r\n"
        if response_headers:
            for key, val in response_headers.items():
                response += f"{key}: {val}\r\n"
        response += "\r\n"
        response = response.encode()
        if datas:
            response += datas

        print(f"[{datetime.utcnow()}] {self.ip} {self.method} {self.route} {code}")

        return response

    def not_found(self):
        html = make_html(
            "404 Not Schtroumpf",
            """\
<body>
    <h1>Not Schtroumpf</h1>
    <p>
        The requested URL was not schtroumpf on the server. If you schtroumpfed the URL manually
        please schtroumpf your spelling and schtroumpf again.
    </p>
</body>""",
        )
        return self.make_response(
            404,
            headers={"Content-Type": "text/html;charset=utf-8"},
            datas=html.encode(),
        )


class HTTPServer:
    """le prochain mini framework web à la mode
    ou pas
    """

    def __init__(self, hostname, port=8080, max_threads=4):
        self.hostname = hostname
        self.port = port
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.server.bind((hostname, port))
        self.max_threads = max_threads

    def run(self):
        self.server.listen(5)
        print(f"server running on {self.hostname}:{self.port}")
        with ThreadPoolExecutor(max_workers=self.max_threads) as pool:
            while True:
                client, client_info = self.server.accept()
                pool.submit(self._handle_request, client, client_info)

    def _parse_headers(self, datas):
        res = {}
        for line in datas.splitlines():
            key, sep, value = line.partition(": ")
            if sep:
                res[key] = value
        return res

    def _handle_request(self, client, client_info):
        datas = bytearray()
        try:
            datas.extend(client.recv(2048))
            command, rn, datas = datas.partition(b"\r\n")
            if not rn:
                client.send(Request().make_response(414))
                return
            while b"\r\n\r\n" not in datas and (chunk := client.recv(2048)):
                datas.extend(chunk)
            headers, rn, datas = datas.partition(b"\r\n\r\n")
            headers = self._parse_headers(headers.decode("ascii"))
            length = int(headers.get("Content-Length", 0))
            while len(datas) < length and (chunk := client.recv(2048)):
                datas.extend(chunk)

            request = Request(command, datas, headers=headers, ip=client_info[0])
            if request.hostname != self.hostname:
                client.send(request.not_found())
                return
            response = self._get_response(request)
            client.send(response)
        except ZeroDivisionError:
            # todo: retrouver comment renvoyer la traceback
            client.send(request.make_response(500))
        finally:
            client.close()

    def _get_response(self, request):
        """flemme de faire un routing map alors...
        il suffira de surchager cette méthode
        """
        if request.method == "HEAD":
            return request.make_response(200)
        return request.not_found()
