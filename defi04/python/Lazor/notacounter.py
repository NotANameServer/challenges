""" Send help I'm trying to reimplement Werkzeug """

import collections
import contextlib
import contextvars
import functools
import itertools
import json
import logging
import operator
import re
import urllib.parse
from http import HTTPStatus


logger = logging.getLogger(__name__)
request_var = contextvars.ContextVar('request')
ACCEPTABLES = {'application/json', 'text/html', 'text/plain'}
SUPPORTED_MEDIA_TYPES = {'application/json', 'x-www-form-urlencoded'}


def tree():
    return collections.defaultdict(tree)


class Disconnect(Exception):
    pass


class HTTPError(Disconnect):
    def __init__(self, code, *args):
        self.code = code
        super().__init__(*args)


class WebRequest:
    def __init__(self, environ, start_response):
        self.environ = environ
        self._start_response = start_response
        self._body = None
        self._form = None

    @staticmethod
    def _parse_header(header):
        # Simpler parsing, does not parse "quoted-string".
        # https://tools.ietf.org/html/rfc2045#section-5.1
        ct, *options = map(str.strip, header.split(';'))
        options = {
            key: value
            for option in options
            for key, value in (option.split('='),)
        }
        return ct, options

    @property
    def body(self):
        if self._body is None:
            try:
                self._body = self.environ['wsgi.input'].read()
            except ConnectionResetError:
                raise Disconnect()
        return self._body

    @property
    def text(self):
        try:
            return self.body.decode(self.content_type[1]['charset'])
        except UnicodeError:
            raise HTTPError(HTTPStatus.BAD_REQUEST, "Cannot parse body.")

    @property
    def content_type(self):
        try:
            ct = self._parse_header(self.environ['CONTENT_TYPE'])
            ct.setdefault('charset', 'ascii')
            return ct
        except KeyError:
            raise HTTPError(HTTPStatus.UNSUPPORTED_MEDIA_TYPE)

    @property
    def accept(self):
        for accept in self.environ.get('HTTP_ACCEPT', '').split(','):
            ct, options = self._parse_header(accept)
            if ct in ACCEPTABLES:
                options.setdefault('charset', 'ascii')
                return ct, options

        # assume plaintext by default
        return 'text/plain', {'charset': 'ascii'}

    @property
    def form(self):
        if self._form is None:
            try:
                self._form = {}
                ct, options = self.content_type
                if ct == 'application/x-www-form-urlencoded':
                    self._form.update(urllib.parse.parse_qs(self.environ['QUERY_STRING']))
                    self._form.update(urllib.parse.parse_qs(self.text))
                elif ct == 'application/json':
                    self._form.update(json.loads(self.text))
                else:
                    raise HTTPError(HTTPStatus.UNSUPPORTED_MEDIA_TYPE)
            except ValueError as exc:
                raise HTTPError(HTTPStatus.BAD_REQUEST)

        return self._form

    def make_response(self, data):
        return WebResponse(self.accept, data)

    def __enter__(self):
        request_var.set(self)

    def __exit__(self, *args):
        request_var.reset()


class WebResponse:
    def __init__(self, accept, data):
        self._chunks = collections.deque()
        self.content_type = accept[0]
        self.charset = accept[1]['charset']
        self._sentinel = object()

    def _dumps(self, data):
        return {
            'application/json': json.dumps,
            'text/plain': pprint.pformat,
            'text/html': html_dumps
        }[self.content_type](data)

    def enqueue(self, data):
        self._chunks.append(self._dumps(data))

    def enqueue_sentinel(self):
        self._chunks.append(self._sentinel)

    def __aiter__(self):
        return self

    async def __anext__(self):
        while not self._chunks:
            await asyncio.sleep(0)
        if self._chunks[0] is self._sentinel:
            raise StopAsyncIteration
        return self._chunks.popleft()


class CounterModel(collections.UserDict):
    def __init__(self):
        super().__init__({'carrote': 10})

    def __getitem__(self, item):
        return self.data[item]

    def __setitem__(self, item, value):
        if item == 'etoile':
            raise KeyError('Cannot set the etoile counter value')
        if self.data[item] < value:
            raise ValueError('Cannot decrease the %r counter value' % item)
        self.data[item] = value
        self.data['etoile'] += value

    def __delitem__(self, item):
        if item == 'etoie':
            raise KeyError('Cannot delete the etoile counter')
        value = self.data.pop(item)
        self.data['etoile'] -= value


counters = CounterModel()


class Controller:
    # _router = {host: {verb: regexp}}
    _router = {}
    # _router_setup = {host: {verb: [pattern, ...]}}
    _router_setup = collections.defaultdict(functools.partial(collections.defaultdict, list))
    _funcmap = {}
    _funcuidgen = map(lambda i: f'-func{i}', itertools.count())

    @classmethod
    def route(cls, path, verbs=['GET']):
        def wrapper(function):
            if not path.startswith('/'):
                raise SyntaxError(f'Invalid path: {path}')
            parts = path[1:].split('/')
            if not all(parts):
                raise SyntaxError(f'Invalid path: {path}')

            # Replace "/<param>/" by "/(?P<param>[^/]*)/" in the path
            for i, part in enumerate(parts):
                if part[0] == '<' and part[-1] == '>':
                    param, _, ttype = part[1:-1].partition(':')
                    if not re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', param):
                        raise SyntaxError(f'Invalid param: {param}')
                    parts[i] = {
                        'str': r'(?P<{}>[^/]+)',
                        'int': r'(?P<{}>\d+)',
                    }[ttype or 'str'].format(param)
                else:
                    parts[i] = re.escape(part)

            # Give the function an unique identifier and save it in the
            # map. Reuse the unique identifier in the pattern used to
            # match this route so it is possible to get the function id
            # back when matching the request path and find the function
            # back.
            funcuid = next(cls._funcuidgen)
            cls._funcmap[funcuid] = function
            pattern = '(?P<{}>/{})'.format(funcuid, '/'.join(parts))

            # Save the route in the router
            for verb in verbs:
                cls._router_setup[cls.host][verb].append(pattern)

            return classmethod(function)
        return wrapper

    @classmethod
    def _setup_then_dispatch(cls, req):
        for host, verbrouter in cls._router_setup.items():
            cls._router[host] = {}
            for verb, patterns in verbrouter.items():
                cls._router[host][verb] = re.compile('^(%s)$' % '|'.join(patterns))
        cls.dispatch = cls._dispatch
        cls.dispatch(req)

    @classmethod
    def _dispatch(self, req):
        verb = req.environ['REQUEST_METHOD']
        path = req.environ['PATH_INFO']
        host = req.environ.get('HTTP_HOST')

        hostrouter = cls._router
        if not (verbrouter := hostrouter.get(host)):
            raise HTTPError(HTTPStatus.NOT_FOUND)
        if not (pathre := verbrouter.get(verb)):
            raise HTTPError(HTTPStatus.METHOD_NOT_ALLOWED)
        if not (match := pathre.match(path)):
            raise HTTPError(HTTPStatus.NOT_FOUND)

        funcuid = next(filter(str.startswith('-func'), match.groupdict()))
        params = match.groupdict()
        params.pop(funcuid)
        await cls._funcmap[funcuid](**params)

    dispatch = _setup_then_dispatch

route = Controller.route

class CounterController(Controller):
    host = 'counter.notaname.fr'

    @route('/', ['GET'])
    async def get_all_counters():
        return list(counters.keys())

    @route('/<counter>', ['GET'])
    async def get_counter(counter):
        return counters[counter]

    @route('/', ['POST'])
    async def new_counter():
        req = request_var.get()
        name = req.form['name']
        value = req.form.get('value', 0)
        if name in counters:
            raise HTTPError(HTTPStatus.BAD_REQUEST)
        counters[name] = value
        return {'name': name, 'value': value}

    @route('/<counter>', ['PUT'])
    async def update_counter(counter):
        req = request_var.get()
        value = req.form.get('value')
        if counter not in counters:
            raise HTTPError(HTTPStatus.NOT_FOUND)
        try:
            counters[counter] = value
        except (ValueError, KeyError) as exc:
            raise HTTPError(HTTPStatus.BAD_REQUEST) from exc
        return {'name': counter, 'value': value}

    @route('/<counter>', ['DELETE'])
    async def delete_counter(counter):
        try:
            del counters[counter]
        except KeyError as exc:
            raise HTTPError(HTTPStatus.NOT_FOUND) from exc


async def application(environ, start_response):
    with contextlib.suppress(Disconnect),\
         WebRequest(environ, start_response) as req:
        try:
            res = await Controller.dispatch(req)
            if not isinstance(res, WebResponse):
                res = req.make_response(res)
        except HTTPError as exc:
            await start_response(f'{exc.code.value} {exc.code.phrase}', [
                ('Content-Length', '0'),
            ])
            yield
        else:
            res.enqueue_sentinel()
            yield from res
