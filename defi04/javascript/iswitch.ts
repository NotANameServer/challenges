import { Server as NetServer, Socket, AddressInfo } from 'net';
import * as qs from 'querystring';

const STATUS_CODES = {
    100: 'Continue',                   // RFC 7231 6.2.1
    101: 'Switching Protocols',        // RFC 7231 6.2.2
    102: 'Processing',                 // RFC 2518 10.1 (obsoleted by RFC 4918)
    103: 'Early Hints',                // RFC 8297 2
    200: 'OK',                         // RFC 7231 6.3.1
    201: 'Created',                    // RFC 7231 6.3.2
    202: 'Accepted',                   // RFC 7231 6.3.3
    203: 'Non-Authoritative Information', // RFC 7231 6.3.4
    204: 'No Content',                 // RFC 7231 6.3.5
    205: 'Reset Content',              // RFC 7231 6.3.6
    206: 'Partial Content',            // RFC 7233 4.1
    207: 'Multi-Status',               // RFC 4918 11.1
    208: 'Already Reported',           // RFC 5842 7.1
    226: 'IM Used',                    // RFC 3229 10.4.1
    300: 'Multiple Choices',           // RFC 7231 6.4.1
    301: 'Moved Permanently',          // RFC 7231 6.4.2
    302: 'Found',                      // RFC 7231 6.4.3
    303: 'See Other',                  // RFC 7231 6.4.4
    304: 'Not Modified',               // RFC 7232 4.1
    305: 'Use Proxy',                  // RFC 7231 6.4.5
    307: 'Temporary Redirect',         // RFC 7231 6.4.7
    308: 'Permanent Redirect',         // RFC 7238 3
    400: 'Bad Request',                // RFC 7231 6.5.1
    401: 'Unauthorized',               // RFC 7235 3.1
    402: 'Payment Required',           // RFC 7231 6.5.2
    403: 'Forbidden',                  // RFC 7231 6.5.3
    404: 'Not Found',                  // RFC 7231 6.5.4
    405: 'Method Not Allowed',         // RFC 7231 6.5.5
    406: 'Not Acceptable',             // RFC 7231 6.5.6
    407: 'Proxy Authentication Required', // RFC 7235 3.2
    408: 'Request Timeout',            // RFC 7231 6.5.7
    409: 'Conflict',                   // RFC 7231 6.5.8
    410: 'Gone',                       // RFC 7231 6.5.9
    411: 'Length Required',            // RFC 7231 6.5.10
    412: 'Precondition Failed',        // RFC 7232 4.2
    413: 'Payload Too Large',          // RFC 7231 6.5.11
    414: 'URI Too Long',               // RFC 7231 6.5.12
    415: 'Unsupported Media Type',     // RFC 7231 6.5.13
    416: 'Range Not Satisfiable',      // RFC 7233 4.4
    417: 'Expectation Failed',         // RFC 7231 6.5.14
    418: 'I\'m a Teapot',              // RFC 7168 2.3.3
    421: 'Misdirected Request',        // RFC 7540 9.1.2
    422: 'Unprocessable Entity',       // RFC 4918 11.2
    423: 'Locked',                     // RFC 4918 11.3
    424: 'Failed Dependency',          // RFC 4918 11.4
    425: 'Too Early',                  // RFC 8470 5.2
    426: 'Upgrade Required',           // RFC 2817 and RFC 7231 6.5.15
    428: 'Precondition Required',      // RFC 6585 3
    429: 'Too Many Requests',          // RFC 6585 4
    431: 'Request Header Fields Too Large', // RFC 6585 5
    451: 'Unavailable For Legal Reasons', // RFC 7725 3
    500: 'Internal Server Error',      // RFC 7231 6.6.1
    501: 'Not Implemented',            // RFC 7231 6.6.2
    502: 'Bad Gateway',                // RFC 7231 6.6.3
    503: 'Service Unavailable',        // RFC 7231 6.6.4
    504: 'Gateway Timeout',            // RFC 7231 6.6.5
    505: 'HTTP Version Not Supported', // RFC 7231 6.6.6
    506: 'Variant Also Negotiates',    // RFC 2295 8.1
    507: 'Insufficient Storage',       // RFC 4918 11.5
    508: 'Loop Detected',              // RFC 5842 7.2
    509: 'Bandwidth Limit Exceeded',
    510: 'Not Extended',               // RFC 2774 7
    511: 'Network Authentication Required' // RFC 6585 6
};

class Subject<T> {

    protected _listeners: ((data: T) => void)[];

    constructor() {
        this._listeners = [];
    }

    subscribe(callback: (data: T) => void): void {
        this._listeners.push(callback);
    }

    async send(data: T) {
        this._listeners.forEach(async (listener) => {
            await listener(data);
        });
    }

}

type Request = {
    method: string,
    uri: string,
    protocol: {
        name: string,
        version: number
    },
    headers: {
        [headerName: string]: string | string[]
    },
    body: { [key: string]: any } | null
};

type Response = {
    protocol: {
        name: string,
        version: number
    },
    status: {
        code: number,
        name: string
    },
    headers: {
        Server: string,
        'Content-Type': string,
        'Content-Encoding': string,
        Connection: string,
        Date: string,
        [headerName: string]: string | string[] | number
    },
    body: { [key: string]: any } | string
};

class Server {

    private _server: NetServer;
    private _host: string;
    private _port: number;
    private _encoding: BufferEncoding;
    private _currentConnections: number;
    private _keepAliveTimeout: number;
    private _keepAliveMaxRequests: number;
    private _keepAliveProbeDelay: number;
    private _onServerStarted: Subject<AddressInfo | string | null>;
    private _onServerClosed: Subject<void>;
    private _onClientConnected: Subject<Partial<AddressInfo>>;
    private _onClientDisconnected: Subject<Partial<AddressInfo>>;
    private _onClientRequestReceived: Subject<{ request: Request, response: Response, client: Partial<AddressInfo> }>;
    private _onClientResponseSent: Subject<{ response: { raw: string, parsed: Response }, client: Partial<AddressInfo>, error?: Error }>;

    public get encoding(): BufferEncoding {
        return this._encoding;
    }

    public set encoding(encoding: BufferEncoding) {
        this._encoding = encoding;
    }

    public get connections(): number {
        return this._currentConnections;
    }

    public get onServerStarted(): Subject<AddressInfo | string | null> {
        return this._onServerStarted;
    }

    public get onServerClosed(): Subject<void> {
        return this._onServerClosed;
    }

    public get onClientConnected(): Subject<Partial<AddressInfo>> {
        return this._onClientConnected;
    }

    public get onClientDisconnected(): Subject<Partial<AddressInfo>> {
        return this._onClientDisconnected;
    }

    public get onClientRequestReceived(): Subject<{ request: Request, response: Response, client: Partial<AddressInfo> }> {
        return this._onClientRequestReceived;
    }

    public get onClientResponseSent(): Subject<{ response: { raw: string, parsed: Response }, client: Partial<AddressInfo>, error?: Error }> {
        return this._onClientResponseSent;
    }

    constructor() {
        this._server = new NetServer();
        this._host = 'localhost';
        this._port = 8080;
        this._encoding = 'utf-8';
        this._currentConnections = 0;
        this._keepAliveTimeout = 5;
        this._keepAliveMaxRequests = 1000;
        this._keepAliveProbeDelay = 1000;
        this._onServerStarted = new Subject<AddressInfo | string | null>();
        this._onServerClosed = new Subject<void>();
        this._onClientConnected = new Subject<Partial<AddressInfo>>();
        this._onClientDisconnected = new Subject<Partial<AddressInfo>>();
        this._onClientRequestReceived = new Subject<{ request: Request, response: Response, client: Partial<AddressInfo> }>();
        this._onClientResponseSent = new Subject<{ response: { raw: string, parsed: Response }, client: Partial<AddressInfo>, error?: Error }>();
        this._initialize();
    }

    public start(port = this._port, host = this._host): void {
        this._port = port;
        this._host = host;
        this._server.listen(this._port, this._host);
    }

    public stop(): void {
        this._server.close();
    }

    private _initialize(): void {
        this._server.on('connection', (socket: Socket) => {
            socket.setEncoding(this._encoding);
            const clientInformation: Partial<AddressInfo> = {
                address: socket.remoteAddress,
                family: socket.remoteFamily,
                port: socket.remotePort
            };
            this._server.getConnections(async (error: Error | null, count: number) => {
                this._currentConnections = count;
                await this.onClientConnected.send(clientInformation);
            });
            socket.on('data', async (data: Buffer) => {
                const request: Request = this._parseRequest(data.toString());
                if (request.headers.Connection === 'keep-alive') {
                    socket.setKeepAlive(true, this._keepAliveProbeDelay);
                }
                const response: Response = this._createBaseResponse(request);
                await this.onClientRequestReceived.send({ request, response, client: clientInformation });
                const rawResponse: string = this._buildRawResponse(request, response);
                socket.write(rawResponse, async (error?: Error) => {
                    await this.onClientResponseSent.send({ response: { raw: rawResponse, parsed: response }, client: clientInformation, error });
                });
                if (request.headers.Connection === 'close') {
                    socket.end();
                }
            });
            socket.on('close', async (hadError: boolean) => {
                this._server.getConnections(async (error: Error | null, count: number) => {
                    this._currentConnections = count;
                    await this.onClientDisconnected.send(clientInformation);
                });
            });
            socket.on('end', async () => {
                this._server.getConnections(async (error: Error | null, count: number) => {
                    this._currentConnections = count;
                    await this.onClientDisconnected.send(clientInformation);
                });
            });
        });
        this._server.on('listening', async () => {
            await this.onServerStarted.send(this._server.address());
        });
        this._server.on('close', async () => {
            await this.onServerClosed.send();
        });
    }

    private _parseRequest(rawRequest: string): Request {
        const requestLines = rawRequest.split('\r\n');
        const parsedRequest: Request = {
            method: '',
            uri: '',
            protocol: {
                name: '',
                version: -1
            },
            headers: {},
            body: null
        };
        const requestFirstLine = requestLines[0].split(' ');
        parsedRequest.method = requestFirstLine[0];
        parsedRequest.uri = requestFirstLine[1];
        parsedRequest.protocol.name = requestFirstLine[2].split('/')[0];
        parsedRequest.protocol.version = Number(requestFirstLine[2].split('/')[1]);
        const bodyStartIndex = requestLines.indexOf('');
        const requestHeaderLines = requestLines.slice(1, bodyStartIndex);
        requestHeaderLines.forEach((requestHeaderLine: string) => {
            const trimmedRequestHeaderLine = requestHeaderLine.replace(/\s/g, '').split(':');
            parsedRequest.headers[trimmedRequestHeaderLine[0]] = trimmedRequestHeaderLine[1].includes(',') ? trimmedRequestHeaderLine[1].split(',') : trimmedRequestHeaderLine[1];
        });
        const requestBodyLines = requestLines.slice(bodyStartIndex + 1);
        const requestBodyInline = requestBodyLines.join('').replace(/\s/g, '');
        switch (parsedRequest.headers['Content-Type']) {
            case 'application/x-www-form-urlencoded':
                parsedRequest.body = JSON.parse(JSON.stringify(qs.parse(requestBodyInline)));
                break;
            case 'application/json':
                parsedRequest.body = JSON.parse(requestBodyInline);
                break;
            default:
                parsedRequest.body = null;
                break;
        }
        return parsedRequest;
    }

    private _createBaseResponse(request: Request): Response {
        const response: Response = {
            protocol: {
                name: 'HTTP',
                version: 1.1
            },
            status: {
                code: 200,
                name: STATUS_CODES[200]
            },
            headers: {
                Server: 'Node',
                'Content-Type': '',
                'Content-Encoding': 'identity',
                Connection: '',
                Date: new Date().toString()
            },
            body: {}
        };
        let contentType: string = '';
        if (Array.isArray(request.headers.Accept)) {
            if (request.headers.Accept.find((acceptedContentType: string) => acceptedContentType.includes('*/*')) || request.headers.Accept.find((acceptedContentType: string) => acceptedContentType.includes('application/json'))) {
                contentType = 'application/json';
            }
            else {
                contentType = 'text/html';
            }
        }
        else {
            if (request.headers.Accept.includes('*/*') || request.headers.Accept.includes('application/json')) {
                contentType = 'application/json';
            }
            else {
                contentType = 'text/html';
            }
        }
        response.headers['Content-Type'] = `${contentType}; charset=${this.encoding}`;
        if (!request.headers.Connection || request.headers.Connection === 'keep-alive') {
            response.headers.Connection = 'keep-alive';
            response.headers['Keep-Alive'] = [`timeout=${this._keepAliveTimeout}`, `max=${this._keepAliveMaxRequests}`];
        }
        else {
            response.headers.Connection = 'close';
        }
        return response;
    }

    private _buildRawResponse(request: Request, response: Response): string {
        const startLine: string = `${response.protocol.name}/${response.protocol.version} ${response.status.code} ${response.status.name}`;
        let body: string = '';
        switch (response.headers['Content-Type'].split('; ')[0]) {
            case 'application/json':
                body = JSON.stringify(response.body) || '';
                break;
            default:
                body = typeof response.body === 'string' ? response.body : JSON.stringify(response.body);
                break;
        }
        response.headers['Content-Length'] = Buffer.byteLength(body, this.encoding);
        response.headers.Date = new Date().toString();
        let header = '';
        Object.keys(response.headers).forEach((headerName: string) => {
            const headerValue = response.headers[headerName];
            header += `${headerName}: ${Array.isArray(headerValue) ? headerValue.join(', ') : headerValue}\r\n`;
        });
        return `${startLine}\r\n${header}\r\n${request.method !== 'HEAD' ? body : ''}`;
    }

}

class Router {

    protected _routes: { [route: string]: (request: Request, response: Response) => void };

    constructor() {
        this._routes = {};
    }

    public add(route: string, behaviour: (request: Request, response: Response) => void): void {
        this._routes[route] = behaviour;
    }

    public remove(route: string): void {
        delete this._routes[route];
    }

    public navigate(route: string, request: Request, response: Response): void {
        if (this._routes[route]) {
            this._routes[route](request, response);
        }
        else {
            response.status = {
                code: 404,
                name: STATUS_CODES[404]
            };
            if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                response.body = {
                    error: {
                        status: 404,
                        type: 'not_found',
                        message: `No resource available at '/${route}'.`
                    }
                };
            }
            else {
                response.body = `<!DOCTYPE html>\r\n`;
                response.body += `<html>\r\n`;
                response.body += `\t<head>\r\n`;
                response.body += `\t\t<meta charset="${response.headers['Content-Type'].split('charset=')[1]}">\r\n`;
                response.body += `\t\t<title>404 Error - NodeJS HTTP Server</title>\r\n`;
                response.body += `\t</head>\r\n`;
                response.body += `\t<body>\r\n`;
                response.body += `\t\t<h1>404 Error - NodeJS HTTP Server</h1>\r\n`;
                response.body += `\t\t<p>No resource available at '/${route}'.</p>\r\n`;
                response.body += `\t</body>\r\n`;
                response.body += `</html>`;
            }
        }
    }
}

class CountersService {

    private _counters: { [counterName: string]: number };

    constructor() {
        this._counters = {};
    }

    public getAll(): { [counterName: string]: number } {
        return this._counters;
    }

    public get(counterName: string): number | undefined {
        return this._counters[counterName];
    }

    public set(counterName: string, counterValue: number): void {
        this._counters[counterName] = counterValue;
    }

    public remove(counterName: string) {
        delete this._counters[counterName];
    }

    public sum(): number {
        let sum = 0;
        for (const counterName of Object.keys(this._counters)) {
            sum += this._counters[counterName];
        }
        return sum;
    }

}

// ============================
// =========== MAIN ===========
// ============================

const server: Server = new Server();
const countersService: CountersService = new CountersService();
countersService.set('carotte', 10);
const router = new Router();
const counterBehaviour: (counterName: string) => (request: Request, response: Response) => void = (counterName: string) => {
    return (request: Request, response: Response) => {
        switch(request.method) {
            case 'HEAD':
            case 'GET':
                if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                    response.body = { [counterName]: countersService.get(counterName) };
                }
                else {
                    response.body = `<!DOCTYPE html>\r\n`;
                    response.body += `<html>\r\n`;
                    response.body += `\t<head>\r\n`;
                    response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                    response.body += `\t\t<title>GET ${counterName} - NodeJS HTTP Server</title>\r\n`;
                    response.body += `\t</head>\r\n`;
                    response.body += `\t<body>\r\n`;
                    response.body += `\t\t<h1>GET ${counterName} - NodeJS HTTP Server</h1>\r\n`;
                    response.body += `\t\t<p>${counterName}: ${countersService.get(counterName)}</p>\r\n`;
                    response.body += `\t</body>\r\n`;
                    response.body += `</html>`;
                }
                break;
            case 'PUT':
                const counterValue = countersService.get(counterName);
                if (request.body && request.body.counterValue && !isNaN(request.body.counterValue) && counterValue && request.body.counterValue > counterValue) {
                    countersService.set(counterName, request.body.counterValue);
                    if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                        response.body = { [counterName]: countersService.get(counterName) };
                    }
                    else {
                        response.body = `<!DOCTYPE html>\r\n`;
                        response.body += `<html>\r\n`;
                        response.body += `\t<head>\r\n`;
                        response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                        response.body += `\t\t<title>PUT ${counterName} - NodeJS HTTP Server</title>\r\n`;
                        response.body += `\t</head>\r\n`;
                        response.body += `\t<body>\r\n`;
                        response.body += `\t\t<h1>PUT ${counterName} - NodeJS HTTP Server</h1>\r\n`;
                        response.body += `\t\t<p>${counterName}: ${countersService.get(counterName)}</p>\r\n`;
                        response.body += `\t</body>\r\n`;
                        response.body += `</html>`;
                    }
                }
                else if (!request.body || !request.body.counterValue || isNaN(request.body.counterValue)) {
                    response.status = {
                        code: 400,
                        name: STATUS_CODES[400]
                    };
                    if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                        response.body = {
                            error: {
                                code: 400,
                                type: 'invalid_body',
                                message: `The body should contain a property 'counterValue' of type number.`
                            }
                        };
                    }
                    else {
                        response.body = `<!DOCTYPE html>\r\n`;
                        response.body += `<html>\r\n`;
                        response.body += `\t<head>\r\n`;
                        response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                        response.body += `\t\t<title>400 Error - NodeJS HTTP Server</title>\r\n`;
                        response.body += `\t</head>\r\n`;
                        response.body += `\t<body>\r\n`;
                        response.body += `\t\t<h1>400 Error - NodeJS HTTP Server</h1>\r\n`;
                        response.body += `\t\t<p>Incorrect HTTP request: the body should contain a property 'counterValue' of type number.</p>\r\n`;
                        response.body += `\t</body>\r\n`;
                        response.body += `</html>`;
                    }
                }
                else {
                    response.status = {
                        code: 409,
                        name: STATUS_CODES[409]
                    };
                    if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                        response.body = {
                            error: {
                                code: 409,
                                type: 'invalid_counter_value',
                                message: `Cannot set a lower value, i.e. ${request.body.counterValue}, for counter ${counterName} which current value is ${countersService.get(counterName)}`
                            }
                        };
                    }
                    else {
                        response.body = `<!DOCTYPE html>\r\n`;
                        response.body += `<html>\r\n`;
                        response.body += `\t<head>\r\n`;
                        response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                        response.body += `\t\t<title>409 Error - NodeJS HTTP Server</title>\r\n`;
                        response.body += `\t</head>\r\n`;
                        response.body += `\t<body>\r\n`;
                        response.body += `\t\t<h1>409 Error - NodeJS HTTP Server</h1>\r\n`;
                        response.body += `\t\t<p>Cannot set a lower value, i.e. ${request.body.counterValue}, for counter ${counterName} which current value is ${countersService.get(counterName)}.</p>\r\n`;
                        response.body += `\t</body>\r\n`;
                        response.body += `</html>`;
                    }
                }
                break;
            case 'DELETE':
                countersService.remove(counterName);
                router.remove(counterName);
                if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                    response.body = { deleted: counterName };
                }
                else {
                    response.body = `<!DOCTYPE html>\r\n`;
                    response.body += `<html>\r\n`;
                    response.body += `\t<head>\r\n`;
                    response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                    response.body += `\t\t<title>DELETE ${counterName} - NodeJS HTTP Server</title>\r\n`;
                    response.body += `\t</head>\r\n`;
                    response.body += `\t<body>\r\n`;
                    response.body += `\t\t<h1>DELETE ${counterName} - NodeJS HTTP Server</h1>\r\n`;
                    response.body += `\t\t<p>${counterName} deleted successfully.</p>\r\n`;
                    response.body += `\t</body>\r\n`;
                    response.body += `</html>`;
                }
                break;
            default:
                response.status = {
                    code: 405,
                    name: STATUS_CODES[405]
                };
                response.body = {
                    error: {
                        status: 405,
                        type: 'invalid_method',
                        message: `Method ${request.method} is invalid for '/${counterName}'.`
                    }
                };
                break;
        }
    };
};
router.add('', (request: Request, response: Response) => {
    switch(request.method) {
        case 'HEAD':
        case 'GET':
            const counters = countersService.getAll();
            if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                response.body = { etoile: countersService.sum(), ...counters };
            }
            else {
                response.body = `<!DOCTYPE html>\r\n`;
                response.body += `<html>\r\n`;
                response.body += `\t<head>\r\n`;
                response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                response.body += `\t\t<title>GET all - NodeJS HTTP Server</title>\r\n`;
                response.body += `\t</head>\r\n`;
                response.body += `\t<body>\r\n`;
                response.body += `\t\t<h1>GET all - NodeJS HTTP Server</h1>\r\n`;
                response.body += `\t\t<p>etoile: ${countersService.sum()}</p>\r\n`;
                Object.keys(counters).forEach((counterName) => {
                    response.body += `\t\t<p>${counterName}: ${counters[counterName]}</p>\r\n`;
                });
                response.body += `\t</body>\r\n`;
                response.body += `</html>`;
            }
            break;
        case 'POST':
            if (request.body) {
                const newCounter = Object.keys(request.body)[0];
                if (!countersService.get(newCounter)) {
                    countersService.set(newCounter, request.body[newCounter]);
                    router.add(newCounter, counterBehaviour(newCounter));
                    if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                        response.body = request.body;
                    }
                    else {
                        const counterName = Object.keys(request.body)[0];
                        response.body = `<!DOCTYPE html>\r\n`;
                        response.body += `<html>\r\n`;
                        response.body += `\t<head>\r\n`;
                        response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                        response.body += `\t\t<title>POST ${counterName} - NodeJS HTTP Server</title>\r\n`;
                        response.body += `\t</head>\r\n`;
                        response.body += `\t<body>\r\n`;
                        response.body += `\t\t<h1>POST ${counterName} - NodeJS HTTP Server</h1>\r\n`;
                        response.body += `\t\t<p>${counterName}: ${request.body[counterName]}</p>\r\n`;
                        response.body += `\t</body>\r\n`;
                        response.body += `</html>`;
                    }
                }
                else {
                    response.status = {
                        code: 409,
                        name: STATUS_CODES[409]
                    };
                    if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                        response.body = {
                            error: {
                                status: 409,
                                type: 'duplicate_counter',
                                message: `Counter ${newCounter} already exists.`
                            }
                        };
                    }
                    else {
                        response.body = `<!DOCTYPE html>\r\n`;
                        response.body += `<html>\r\n`;
                        response.body += `\t<head>\r\n`;
                        response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                        response.body += `\t\t<title>409 Error - NodeJS HTTP Server</title>\r\n`;
                        response.body += `\t</head>\r\n`;
                        response.body += `\t<body>\r\n`;
                        response.body += `\t\t<h1>409 Error - NodeJS HTTP Server</h1>\r\n`;
                        response.body += `\t\t<p>Counter ${newCounter} already exists.</p>\r\n`;
                        response.body += `\t</body>\r\n`;
                        response.body += `</html>`;
                    }
                }
            }
            else {
                response.status = {
                    code: 400,
                    name: STATUS_CODES[400]
                };
                if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                    response.body = {
                        error: {
                            code: 400,
                            type: 'invalid_body',
                            message: `The body should contain a the counter name with its associated value.`
                        }
                    };
                }
                else {
                    response.body = `<!DOCTYPE html>\r\n`;
                    response.body += `<html>\r\n`;
                    response.body += `\t<head>\r\n`;
                    response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                    response.body += `\t\t<title>400 Error - NodeJS HTTP Server</title>\r\n`;
                    response.body += `\t</head>\r\n`;
                    response.body += `\t<body>\r\n`;
                    response.body += `\t\t<h1>400 Error - NodeJS HTTP Server</h1>\r\n`;
                    response.body += `\t\t<p>Incorrect HTTP request: the body should contain a the counter name with its associated value.</p>\r\n`;
                    response.body += `\t</body>\r\n`;
                    response.body += `</html>`;
                }
            }
            break;
        default:
            response.status = {
                code: 405,
                name: STATUS_CODES[405]
            };
            if (response.headers['Content-Type'].split('; ').includes('application/json')) {
                response.body = {
                    error: {
                        status: 405,
                        type: 'invalid_method',
                        message: `Method ${request.method} is invalid for '/'.`
                    }
                };
            }
            else {
                response.body = `<!DOCTYPE html>\r\n`;
                response.body += `<html>\r\n`;
                response.body += `\t<head>\r\n`;
                response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
                response.body += `\t\t<title>405 Error - NodeJS HTTP Server</title>\r\n`;
                response.body += `\t</head>\r\n`;
                response.body += `\t<body>\r\n`;
                response.body += `\t\t<h1>405 Error - NodeJS HTTP Server</h1>\r\n`;
                response.body += `\t\t<p>Method ${request.method} is invalid for '/'.</p>\r\n`;
                response.body += `\t</body>\r\n`;
                response.body += `</html>`;
            }
            break;
    }
});
router.add('carotte', counterBehaviour('carotte'));
router.add('etoile', (request: Request, response: Response) => {
    if (request.method === 'GET' || request.method === 'HEAD') {
        if (response.headers['Content-Type'].split('; ').includes('application/json')) {
            response.body = {
                etoile: countersService.sum()
            };
        }
        else {
            response.body = `<!DOCTYPE html>\r\n`;
            response.body += `<html>\r\n`;
            response.body += `\t<head>\r\n`;
            response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
            response.body += `\t\t<title>GET etoile - NodeJS HTTP Server</title>\r\n`;
            response.body += `\t</head>\r\n`;
            response.body += `\t<body>\r\n`;
            response.body += `\t\t<h1>GET etoile - NodeJS HTTP Server</h1>\r\n`;
            response.body += `\t\t<p>etoile: ${countersService.sum()}</p>\r\n`;
            response.body += `\t</body>\r\n`;
            response.body += `</html>`;
        }
    }
    else {
        response.status = {
            code: 405,
            name: STATUS_CODES[405]
        };
        if (response.headers['Content-Type'].split('; ').includes('application/json')) {
            response.body = {
                error: {
                    status: 405,
                    type: 'invalid_method',
                    message: `Method ${request.method} is invalid for '/etoile'.`
                }
            };
        }
        else {
            response.body = `<!DOCTYPE html>\r\n`;
            response.body += `<html>\r\n`;
            response.body += `\t<head>\r\n`;
            response.body += `\t\t<meta charset="${server.encoding}">\r\n`;
            response.body += `\t\t<title>405 Error - NodeJS HTTP Server</title>\r\n`;
            response.body += `\t</head>\r\n`;
            response.body += `\t<body>\r\n`;
            response.body += `\t\t<h1>405 Error - NodeJS HTTP Server</h1>\r\n`;
            response.body += `\t\t<p>Method ${request.method} is invalid for '/etoile'.</p>\r\n`;
            response.body += `\t</body>\r\n`;
            response.body += `</html>`;
        }
    }
});
server.onServerStarted.subscribe((address: string | AddressInfo | null) => {
    if (address) {
        console.log(`Server started on ${typeof address === 'string' ? address : `port ${address.port}`}.`);
    }
    else {
        console.log('Server started.');
    }
});
server.onServerClosed.subscribe(() => {
    console.log('Server closed.');
});
server.onClientConnected.subscribe((clientInformation: Partial<AddressInfo>) => {
    console.log(`A new client reached the server:`, clientInformation);
    console.log(`There are now ${server.connections} clients on the server.`);
});
server.onClientRequestReceived.subscribe(({ request, response, client }) => {
    console.log(`A new request has been received from client ${JSON.stringify(client)}`);
    console.log(request);
    const uriParts = request.uri.slice(1).split('/');
    router.navigate(uriParts[0], request, response);
});
server.onClientResponseSent.subscribe(({ response, client, error}) => {
    console.log(`Responded to client ${JSON.stringify(client)} with:`);
    console.log(response.raw);
});
server.onClientDisconnected.subscribe((client: Partial<AddressInfo>) => {
    console.log(`Client ${JSON.stringify(client)} left the server.`);
    console.log(`There are now ${server.connections} clients on the server.`);
});
server.start();
