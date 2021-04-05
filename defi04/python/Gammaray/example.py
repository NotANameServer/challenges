import webserver


app = webserver.WebApp(port=8080)
app.init()

counters = { 'carrot': 10 }
etoile = lambda: len(counters)


@app.load_path(["", "star"])
def home():
	app.response.headers["Content-type"] = "text/html"
	return f"{etoile()} counters"


@app.load_path([""], method="POST")
def post_counter():
	if app.request.body != "":
		app.response.headers["Content-type"] = "json"
		counters[app.request.body] = 0
		return "{" + app.request.body + ": 0}"
	else:
		app.response.headers["Content-type"] = "text/plain"
		return "Le nouveau compteur doit avoir un nom"


@app.load_path(counters.keys())
def get_counter():
	path = app.request.path
	counter_name = path[1:]

	app.response.headers["Content-type"] = "text/html"
	return f"{counters[counter_name]} {counter_name}"


@app.load_path(counters.keys(), method="PUT")
def add_counter():
	name = app.request.path[1:]

	app.response.headers["Content-type"] = "text/plain"

	if name in counters.keys():
		counters[name] = app.request.body if app.request.body != "" else counters[name] + 1
		return "Done"
	else:
		return "Ce compteur n'existe pas"


@app.load_path(counters.keys(), method="DELETE")
def del_counter():
	path = app.request.path

	if path[1:] in counters.keys():
		counters.pop(path[1:])

	app.response_type = "text/html"
	return ""


app.run(keep_log=True)
