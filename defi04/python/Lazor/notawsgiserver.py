#!/usr/bin/env python3

""" A minimalist asynchronous WSGI server """

NAME = __file__[:-3]
VERSION = (0, 0, 0)

import argparse
import asyncio
import binascii
import logging
import re
import sys
import urllib.parse
import warnings
from http import HTTPStatus

logger = logging.getLogger(__name__)
SENTINEL = object()

rfc2047_re = re.compile(r'\b=\?([\w-_]+)\?([qQbB])\?([^\s]*)\?=\b')
def rfc2047_re_repl(match):
    a2b = binascii.a2b_qp if match.group(2) in 'qQ' else binascii.a2b_base64
    return a2b(match.group(3)).decode(match.group(1))


async def aiter(coro, sentinel):
    while True:
        value = await coro()
        if value == sentinel:
            raise StopIteration()
        yield value


def main():
    """
    Application entrypoint, extract configuration from argparse,
    configure logging and run the server loop.
    """

    cli = argparse.ArgumentParser()
    cli.add_argument('wsgi-app')
    cli.add_argument('--host', type=str, default='::1')
    cli.add_argument('--port', type=int, default=8080)
    cli.add_argument('-v', '--verbose', action='count', default=0)
    cli.add_argument('-s', '--silent', action='count', default=0)
    options = cli.parse_args()

    # Color the [LEVEL] part of messages, need new terminal on Windows
    # https://github.com/odoo/odoo/blob/13.0/odoo/netsvc.py#L57-L100
    class ColoredFormatter(logging.Formatter):
        colors = {
            logging.DEBUG: (34, 49),
            logging.INFO: (32, 49),
            logging.WARNING: (33, 49),
            logging.ERROR: (31, 49),
            logging.CRITICAL: (37, 41),
        }
        def format(self, record):
            fg, bg = type(self).colors.get(record.levelno, (32, 49))
            record.levelname = f"\033[1;{fg}m\033[1;{bg}m{record.levelname}\033[0m"
            return super().format(record)

    # Reset logging to ONLY log messages on stdout
    stdout = logging.StreamHandler()
    stdout.formatter = ColoredFormatter(
        "%(asctime)s [%(levelname)s] <%(funcName)s> %(message)s"
    )
    logging.root.handlers.clear()
    logging.root.addHandler(stdout)

    # Set the verbosity, enables python level warnings on -vvv
    verbosity = 10 * max(0, min(3 - options.verbose + options.silent, 5))
    logging.root.setLevel(verbosity)
    if verbosity == 0:
        logging.captureWarnings(True)
        warnings.filterwarnings('default')

    # Find the application
    module_name, variable_name = options['wsgi-app'].split(':')
    app = getattr(importlib.import_module(module_name), variable_name)

    # Start the server on foreground, gracefully quit on the first SIGINT
    server = Server(options.host, options.port, app)
    loop = asyncio.new_event_loop()
    loop.create_task(server.serve())
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        print("Press ctrl-c again to force exit.")
        loop.run_until_complete(server.quit())
        loop.run_until_complete(loop.shutdown_asyncgens())
        loop.close()
    except Exception:
        logger.critical("Fatal error in server loop !", exc_info=True)
        raise SystemExit(1)


class BufferedLineReader:
    """ Fix asyncio.StreamReader to implement an API similar to io. """
    def __init__(self, reader, newline=b"\n"):
        self.reader = reader
        self.newline = newline
        self._buffer = bytearray()

    async def read(self, size=-1):
        if size is None or size < 0:
            buf, self._buffer = self._buffer, bytearray()
            buf.extend(await self.reader.read())
            return buf

        buf, self._buffer = self._buffer[:size], self._buffer[size:]
        return buf + await self.reader.read(len(buf) - size)

    async def readline(self, size=-1):
        line, newline, rest = self._buffer.partition(self.newline)
        if not newline:
            chunk = await self.reader.read(size - len(line))
            self._buffer.extend(chunk)
            line, newline, rest = self._buffer.partition(self.newline)
        self._buffer = rest
        return line + newline

    def __getattr__(self, attr, default=SENTINEL):
        if hasattr(io.BufferedReader, attr):
            raise ValueError('Unsupported operation')
        return getattr(super(), attr, *[
            d for d in (default,) if d is not SENTINEL
        ])


class Server:
    def __init__(self, host, port, app):
        self.host = host
        self.port = port
        self.app = app
        self._serv = None
        self._clients = set()
        self.base_environ = {
            'wsgi.errors': sys.stderr,
            'wsgi.version': (1, 0),
            'wsgi.multithread': False,
            'wsgi.multiprocess': False,
            'wsgi.run_once': False,
            'wsgi.input_terminated': True,
            'wsgi.url_scheme': 'http'
            'SERVER_NAME': self.host,
            'SERVER_PORT': self.port,
            'SERVER_PROTOCOL': 'HTTP/1.0',
            'SERVER_SOFTWARE': __file__[:-3] + '-' + '.'.join(VERSION),
            'SCRIPT_NAME': '',
        }

    async def serve(self):
        """ Start listening for new connections """
        self._serv = await asyncio.start_server(self.handle, self.host, self.port)
        logger.info("Listening on %s port %i", self.host, self.port)

    async def quit(self):
        logger.info("Terminating all connections...")
        coros = []
        for sock in self.local.users():
            sock.writer.write_eof()
            coros.append(sock.writer.wait_closed())
        if coros:
            await asyncio.wait(coros)
        self._serv.close()
        await self._serv.wait_closed()

    async def handle(self, reader, writer):
        """ Create a new Client and serve him until a response is sent """
        peeraddr, peerport = writer.get_extra_info('peername')
        logger.debug("New connection from %s", peeraddr)
        client = Client(reader, writer)
        self._clients.add(client)
        try:
            await client.serve(self.app, self.base_environ.copy())
        except Disconnect as exc:
            logger.warning("Error while parsing request.", exc_info=True)
        except Exception:
            logger.exception("Error in client loop !")
        self._clients.discard(client)
        writer.close()
        await writer.wait_closed()
        logger.debug("Connection with %s closed", peeraddr)


class Disconnect(Exception):
    pass

class Client:
    def __init__(self, reader, writer):
        self.reader = BufferedLineReader(reader, newline=b"\r\n")
        self.writer = writer
        peername = writer.get_extra_info('peername')
        self.peeraddr = peername[0]
        self.peerport = peername[1]

    def __str__(self):
        return self.peeraddr

    async def die(self, code):
        await self.start_response(f'{code.value} {code.phrase}', [
            ('Content-Length', '0')
        ])
        self.writer.write_eof()
        await self.writer.drain()
        raise Disconnect()

    async def start_response(self, status, headers):
        self.writer.write(f'HTTP/1.0 {status}\r\n'.encode('ISO-8859-1'))
        await self.writer.drain()
        if headers:
            self.writer.write('\r\n'.join(map(': '.join, headers)).encode())
            self.writer.write(b'\r\n')
        self.writer.write(b'\r\n')
        await self.writer.drain()

    async def _read_command(self, environ):
        try:
            command = await self.reader.readline(size=1024).decode('ascii')
            if not command.endswith('\r\n'):
                self.die(HTTPStatus.REQUEST_URI_TOO_LONG)
            verb, version, uristr = command.decode('ascii').split()
            uri = urllib.parse.urlparse(uristr, allow_fragments=False)
        except ConnectionResetError:
            raise Disconnect() from exc
        except (UnicodeDecodeError, ValueError) as exc:
            raise Disconnect() from exc
        if version > 'HTTP/1.1':
            self.die(HTTPStatus.HTTP_VERSION_NOT_SUPPORTED)

        environ['REQUEST_METHOD'] = verb
        environ['PATH_INFO'] = uri.path
        environ['QUERY_STRING'] = uri.query

    async def _read_headers(self, environ):
        async def readline():
            try:
                line = await self.reader.readline(size=1024).decode('ascii')
                if not line.endswith('\r\n'):
                    self.die(HTTPStatus.REQUEST_HEADER_FIELDS_TOO_LARGE)
                key, value = map(str.strip, line.split(':', 1))
                value = rfc2047_re.sub(rfc2047_re_repl, value)
                return key, value
            except ConnectionResetError:
                raise Disconnect() from exc
            except (UnicodeError, ValueError):
                self.die(HTTPStatus.BAD_REQUEST)

        async for key, value in aiter(readline, ''):
            header = header.replace('-', '_').upper()
            if header in ('CONTENT_TYPE', 'CONTENT_LENGTH'):
                environ[header] = value
            else:
                environ['HTTP_' + header] = value

    async def _app_dispatch(self, app, environ)
        try:
            async for chunk in app(environ, self.start_response):
                self.writer.write(chunk)
                await self.writer.drain()
        except Exception:
            logger.exception("Internal error during application dispatch.")
            self.die(HTTPStatus.INTERNAL_SERVER_ERROR)


    async def serve(self, app, environ):
        environ['wsgi.input'] = self.reader
        self._read_command(environ)
        self._read_headers(environ)
        self._app_dispatch(app, environ)
        self.writer.write_eof()
        await self.writer.drain()


if __name__ == "__main__":
    main()
