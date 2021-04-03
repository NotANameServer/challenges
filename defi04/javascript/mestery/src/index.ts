import { parse } from 'querystring';
import { Server, decodeBody } from 'not-an-httpserver';

const counters = new Map<string, number>([['carotte', 10]]);
// eslint-disable-next-line sonarjs/cognitive-complexity
const server = new Server(async (request, response) => {
	if (request.rawHeaders.host !== 'compteur.notaname.fr:8080') {
		response.writeHead(400);
		response.end();
		return;
	}

	response.writeHead(200);
	const body = request.body ? (decodeBody(request.body, 'utf8') as string) : undefined;

	if (request.method === 'GET' && request.url.pathname === '/') {
		for (let i = 0; i < 2e8; i++) {} // 200 ms
		await new Promise((resolve) => setTimeout(resolve, 300)); // 300 ms

		response.headers['Content-Type'] = 'application/json';
		response.end(
			JSON.stringify({
				...Object.fromEntries(counters.entries()),
				// eslint-disable-next-line unicorn/no-array-reduce
				etoile: [...counters.values()].reduce((accumulator, current) => accumulator + current),
			}),
		);
	} else if (request.method === 'GET' && request.url.pathname && /^\/[\da-z]+$/.test(request.url.pathname)) {
		const counterName = request.url.pathname.slice(1);
		if (counterName === 'etoile') {
			response.headers['Content-Type'] = 'application/json';
			response.end(
				JSON.stringify({
					name: 'etoile',
					// eslint-disable-next-line unicorn/no-array-reduce
					value: [...counters.values()].reduce((accumulator, current) => accumulator + current),
				}),
			);
			return;
		}

		const counter = counters.get(counterName);
		if (counter === undefined) {
			response.writeHead(404);
			response.end();
			return;
		}

		response.headers['Content-Type'] = 'application/json';
		response.end(JSON.stringify({ name: counterName, value: counter }));
	} else if (request.method === 'POST' && request.url.pathname === '/') {
		let names;
		if (body) {
			try {
				names =
					request.rawHeaders['content-type'] === 'application/x-www-form-urlencoded'
						? parse(body).name
						: JSON.parse(body)?.name;
				// eslint-disable-next-line no-empty
			} catch {}
		}

		if (!names || names.length === 0) {
			response.writeHead(400);
			response.end();
			return;
		}

		if (typeof names === 'string') {
			names = [names];
		}

		const createdCounters: Record<string, number> = {};
		for (const name of names) {
			if (!name.trim() || counters.has(name)) {
				response.writeHead(422, 'Unprocessable Entity');
				response.end();
				return;
			}

			counters.set(name, 0);
			createdCounters[name] = 0;
		}
		response.headers['Content-Type'] = 'application/json';
		response.end(JSON.stringify(createdCounters));
	} else if (request.method === 'PUT' && request.url.pathname && /^\/[\da-z]+$/.test(request.url.pathname)) {
		const counterName = request.url.pathname.slice(1);
		if (counterName === 'etoile') {
			response.writeHead(405, 'Not Allowed', { Accept: 'GET' });
			response.end();
			return;
		}

		if (!counters.has(counterName)) {
			response.writeHead(404);
			response.end();
			return;
		}

		if (!body) {
			response.writeHead(400);
			response.end();
			return;
		}

		let newValue;

		try {
			newValue =
				request.rawHeaders['content-type'] === 'application/x-www-form-urlencoded'
					? parse(body).value
					: JSON.parse(body)?.value;

			newValue = parseInt(Array.isArray(newValue) ? newValue[0] : newValue, 10);
			// eslint-disable-next-line no-empty
		} catch {}

		if (!newValue || isNaN(newValue)) {
			response.writeHead(400);
			response.end();
			return;
		}

		counters.set(counterName, newValue);
		response.writeHead(204);
		response.end();
	} else if (request.method === 'DELETE' && request.url.pathname && /^\/[\da-z]+$/.test(request.url.pathname)) {
		const counterName = request.url.pathname.slice(1);
		if (counterName === 'etoile') {
			response.writeHead(405, 'Not Allowed', { Accept: 'GET' });
			response.end();
			return;
		}

		const deleted = counters.delete(counterName);
		if (!deleted) {
			response.writeHead(404);
			response.end();
			return;
		}

		response.writeHead(204);
		response.end();
	} else {
		response.writeHead(404);
		response.end();
	}
});

server.listen(8080, () => console.log('Connected!'));
