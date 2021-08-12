import os
import re
import time
import json
import server


def render_html(datas):
    """ un convertisseur d'objet jsonifiable en html pas du tout testé """
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
    """ pareil en jsonifiable vers texte """
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


def render_datas(datas, request, title="Schroumpfed !", content_type=None):
    """
    converti datas selon le header Accept de la requête
    content_type peut forcer le format de rendu
    title ne sert que pour l'html
    """
    headers = {}
    if not content_type:
        for accept in request.accepts:
            if accept in ("application/json", "text/html", "text/plain"):
                content_type = accept
                break
        else:
            content_type = "text/plain"
    headers["Content-Type"] = content_type + ";charset=utf-8"

    if content_type == "application/json":
        datas = json.dumps(datas)
    elif content_type == "text/html":
        datas = render_html(datas)
    else:
        datas = render_text(datas)

    if content_type == "text/html":
        datas = server.make_html(title, datas)

    return datas.encode(), headers


class Counters:
    """ un dico en moins bien """

    def __init__(self, data=None, magic=None):
        self._data = dict(data or {})
        self.magic = magic

    def is_magic(self, key):
        return self.magic and key == self.magic

    def __getitem__(self, key):
        if self.is_magic(key):
            return sum(self._data.values())
        return self._data.get(key)

    def __setitem__(self, key, value):
        if self.is_magic(key):
            raise KeyError("Can't set magic counter")
        self._data[key] = value

    def pop(self, key):
        return self._data.pop(key)

    def __contains__(self, key):
        return key in self._data or self.is_magic(key)

    def items(self):
        if self.magic:
            yield self.magic, self[self.magic]
        for k, v in self._data.items():
            yield k, v


class CounterServer(server.HTTPServer):
    """ l'app """

    def __init__(self, hostname, port=8080):
        super().__init__(hostname, port)
        self.counters = Counters({"carotte": 10}, magic="etoile")

    def slow_down(self):
        for _ in range(10_000_000):
            pass
        time.sleep(0.3)

    def _get_response(self, request):
        if request.route == "/favicon.ico" and request.method == "GET":
            return self.favicon(request)
        self.slow_down()
        if request.route == "/":
            if request.method == "GET":
                return self.get_all(request)
            if request.method == "POST":
                return self.post_counter(request)
        elif (match := re.fullmatch(r"/([^/]+)", request.route.rstrip("/"))) :
            if request.method == "GET":
                return self.get_counter(request, match.group(1))
            elif request.method == "PUT":
                return self.put_counter(request, match.group(1))
            elif request.method == "DELETE":
                return self.delete_counter(request, match.group(1))
        return super()._get_response(request)

    def favicon(self, request):
        """ GET /favicon.ico """
        with open(os.path.join(os.path.dirname(__file__), "favicon.ico"), "rb") as f:
            icon = f.read()
        return request.make_response(
            200, datas=icon, headers={"Content-Type": "image/x-icon"}
        )

    def get_all(self, request):
        """ GET / """
        datas, headers = render_datas(
            [{"name": k, "value": v} for k, v in self.counters.items()], request
        )
        return request.make_response(200, datas=datas, headers=headers)

    def get_counter(self, request, key):
        """ GET /<counter> """
        if key in self.counters:
            datas, headers = render_datas(
                {"name": key, "value": self.counters[key]}, request
            )
            return request.make_response(200, datas=datas, headers=headers)
        return request.not_found()

    def post_counter(self, request):
        """ POST / """
        key = request.data.get("name")
        if (
            not key
            or not re.fullmatch(r"[a-zA-Z0-9_]+", key)
            or key in self.counters
            or self.counters.is_magic(key)
        ):
            return request.make_response(400)
        self.counters[key] = 0
        datas, headers = render_datas({"name": request.data, "value": 0}, request)
        return request.make_response(200, datas=datas, headers=headers)

    def put_counter(self, request, key):
        """ PUT /<counter> """
        if key not in self.counters:
            return request.not_found()
        if self.counters.is_magic(key):
            return request.make_response(400)
        value = request.data.get("value", "")
        try:
            value = int(value)
        except ValueError:
            return request.make_response(400)
        code = 204
        if self.counters[key] > value:
            code = 400
        elif self.counters[key] < value:
            self.counters[key] = value
            code = 201
        return request.make_response(code, headers={"Content-Location": f"/{key}"})

    def delete_counter(self, request, key):
        """ DELETE /<counter> """
        try:
            self.counters.pop(key)
        except KeyError:
            return request.not_found()
        return request.make_response(200)


def main():
    server = CounterServer("compteur.notaname.fr", 8080)
    server.run()


main()
