import json
import socketserver

counters = {
    "carotte":10
}

class JSonEncoder:
    def encode(self, data):
        return json.dumps(data)

    def decode(self, data):
        return json.loads(data)

    accept_header = "application/json"
    content_type_header = "application/json"

class HtmlEncoder:
    def encode(self, data):
        options = []
        for k, v in data.items():
            options.append(str(k) + "=" + str(v))

        return "&".join(options)

    def decode(self, data):
        options = {}
        for option in data.split("&"):
            k,v = option.split("=")
            options[k] = v

        return options

    content_type_header = "application/x-www-form-urlencoded"
    accept_header = "text/html"

encoders = [JSonEncoder(), HtmlEncoder()]
default_encoder = encoders[0]

class HttpResponse:
    def build_ok(body = None):
        response = []
        response.append("HTTP/1.1 200 OK")
        response.append("Connection: close")
        response.append("")
        if body:
            response.append(body)

        return "\r\n".join(response)

    def build_notfound(body = None):
        response = []
        response.append("HTTP/1.1 404 Not Found")
        response.append("Connection: close")
        response.append("")
        if body:
            response.append(body)

        return "\r\n".join(response)

    def build_badrequest(body = None):
        response = []
        response.append("HTTP/1.1 400 Bad Request")
        response.append("Connection: close")
        response.append("")
        if body:
            response.append(body)

        return "\r\n".join(response)

class Handler(socketserver.BaseRequestHandler):
    def handle(self):
        data = self.request.recv(2048).decode("utf-8")
        headers = data.split("\r\n")
        self.method, self.uri, self.version = headers[0].split(" ")

        self.headers = {}
        for i, header in enumerate(headers[1:]):
            if (header == ""):
                self.body = "\r\n".join(headers[i+2:])
                break

            key, value = header.split(":", 1)
            key = key.lower()
            value = value.strip()
            self.headers[key] = value

        accept = self.headers.get("accept")
        if accept is not None:
            for encoder in encoders:
                if accept in encoder.accept_header:
                    self.encoder = encoder
                    break
            else:
                self.encoder = default_encoder
        else:
            self.decoder = default_encoder

        content_type = self.headers.get("content-type")
        if content_type is not None:
            for encoder in encoders:
                if content_type in encoder.content_type_header:
                    self.decoder = encoder
                    break
            else:
                self.decoder = default_encoder
        else:
            self.decoder = default_encoder

        methods = {
            "GET": self.handle_get,
            "HEAD": self.handle_head,
            "POST": self.handle_post,
            "PUT": self.handle_put,
            "DELETE": self.handle_delete
        }

        method = methods[self.method]
        if method:
            response = method()
        else:
            return HttpResponse.build_notfound()

        if response is not None:
            self.request.send(response.encode())

    def handle_get(self):
        if (self.uri == "/"):
            return HttpResponse.build_ok(self.encoder.encode(counters))
        else:
            counter_name = self.uri[1:]
            counter = counters.get(counter_name)
            if counter is not None:
                return HttpResponse.build_ok(self.encoder.encode(counter))
            else:
                return HttpResponse.build_notfound()

    def handle_head(self):
        return HttpResponse.build_ok()

    def handle_post(self):
        params = self.decoder.decode(self.body)
        name = params.get("name")
        value = params.get("value", 0)

        if not name:
            return HttpResponse.build_badrequest("Missing name parameter")

        if counters.get(name) is not None:
            return HttpResponse.build_badrequest("Counter " + name + " already exists")

        counters[name] = value
        return HttpResponse.build_ok()

    def handle_put(self):
        counter_name = self.uri[1:]
        counter = counters.get(counter_name)
        if counter is None:
            return HttpResponse.build_notfound()

        params = self.decoder.decode(self.body)
        value = params.get("value", None)
        if value is None:
            return HttpResponse.build_badrequest("Missing value parameter")

        counters[counter_name] = value
        return HttpResponse.build_ok()
    
    def handle_delete(self):
        counter_name = self.uri[1:]
        counter = counters.get(counter_name)
        if counter:
            counters[counter_name] = None
            return HttpResponse.build_ok()
        else:
            return HttpResponse.build_notfound()

HOST, PORT = "localhost", 12345

with socketserver.TCPServer((HOST, PORT), Handler) as server:
    server.serve_forever()

# Note que ceci ne s'affichera jamais
print("J'adore le Python !")
