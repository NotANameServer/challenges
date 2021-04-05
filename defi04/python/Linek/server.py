from datetime import datetime
import json
import socket
import threading

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


def render_html(datas):
    """ un convertisseur json to html pas du tout testé """
    if isinstance(datas, list):
        res = ""
        for data in datas:
            res += f"<div>{render_html(data)}</div>\n"
        return res
    if isinstance(datas, dict):
        res = "<div>"
        for key, data in datas.items():
            res += f"<strong>{key}:</strong> {render_html(data)}, "
        return res.rstrip(", ") + "</div>\n"
    return f"{datas}"


def render_text(datas):
    """ pareil en json to text """
    if isinstance(datas, list):
        res = ""
        for data in datas:
            res += render_text(data) + "\n"
        return res
    if isinstance(datas, dict):
        res = ""
        for key, data in datas.items():
            res += f"{key}: {render_text(data)}, "
        return res.rstrip(", ")
    return f"{datas}"


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
        if content_type == "text/x-www-form-urlencoded":
            self.data = self._parse_url_encoded(self.raw.decode(value or "utf-8"))
        elif content_type == "application/json":
            self.data = json.loads(self.raw.decode(value or "utf-8"))
        else:
            self.data = {}

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
            "Connection": "keep-alive",
        }

    def make_response(
        self,
        code=200,
        content_type=None,
        headers=None,
        datas=None,
        render=True,
        title="Schtroumpfed !",
    ):
        """cette fonction fait trop de trucs :thinking:
        le param render ne sert que si datas est truthy
        le param title ne sert que si on renvoie de l'html
        """
        heads = self._default_headers()
        heads.update(headers or {})

        if datas:

            if not content_type:
                for accept in self.accepts:
                    if accept in ("application/json", "text/html", "text/plain"):
                        content_type = accept
                        break
                else:
                    content_type = "text/plain"
            heads["Content-Type"] = content_type

            if render and isinstance(datas, (list, dict, int, float, bool)):
                if content_type == ("application/json"):
                    datas = json.dumps(datas).encode()
                elif content_type == ("text/html"):
                    datas = render_html(datas)
                else:
                    datas = render_text(datas)

            if content_type == "text/html":
                datas = make_html(title, datas)

            if isinstance(datas, str):
                datas = datas.encode()
            elif not isinstance(datas, bytes):
                datas = str(datas).encode()
            heads["Content-Length"] = len(datas)
        response = f"HTTP/1.0 {code} {HTTP_CODES[code]}\r\n"
        if heads:
            for key, val in heads.items():
                response += f"{key}: {val}\r\n"
        response += "\r\n"
        response = response.encode()
        if datas:
            response += datas

        print(f"[{datetime.utcnow()}] {self.ip} {self.method} {self.route} {code}")

        return response

    def not_found(self):
        return self.make_response(
            404,
            content_type="text/html",
            title="404 Not Schtroumpf",
            datas="""\
<body>
    <h1>Not Schtroumpf</h1>
    <p>
        The requested URL was not schtroumpf on the server. If you schtroumpfed the URL manually
        please schtroumpf your spelling and schtroumpf again.
    </p>
</body>""",
            render=False,
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
        self.semaphore = threading.Semaphore(max_threads)

    def run(self):
        self.server.listen(100)
        print(f"server running on {self.hostname}:{self.port}")
        while True:
            client, client_info = self.server.accept()
            t = threading.Thread(
                target=self._handle_request, args=(client, client_info)
            )
            t.start()

    def _parse_headers(self, datas):
        res = {}
        for line in datas.splitlines():
            key, sep, value = line.partition(": ")
            if sep:
                res[key] = value
        return res

    def _handle_request(self, client, client_info):
        self.semaphore.acquire()
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
            self.semaphore.release()

    def _get_response(self, request):
        """flemme de faire un routing map alors...
        il suffira de surchager cette méthode
        """
        if request.method == "HEAD":
            return request.make_response(200)
        return request.not_found()
