//
// Created by Romain Neil on 26/03/2021.
//

#include "HttpServer.h"

#include <utility>

HttpServer::HttpServer(int port, std::string  bindAddress, std::string bindHost) : m_port(port), m_bindAddr(std::move(bindAddress)), m_bindHost(std::move(bindHost)) {
#if defined(_WIN32)
	WSADATA WSAData;
	WSAStartup(MAKEWORD(2,2), &WSAData);
#endif

	masterSocket = socket(AF_INET, SOCK_STREAM, 0);

	if(masterSocket == INVALID_SOCKET) {
		perror("failed to bind");

#if defined(_WIN32)
		const DWORD lastError = WSAGetLastError();

		puts("error: " + lastError);
#endif
	}

	sockaddr_in sin = {0};

	//Bind to specific ip address, or to 127.0.0.1 by default
	if(bindAddress.empty()) {
		sin.sin_addr.s_addr = htonl(INADDR_ANY);
	} else {
		InetPton(AF_INET, m_bindAddr.c_str(), &sin.sin_addr.s_addr);
	}

	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);

	if(bind(masterSocket, reinterpret_cast<sockaddr*>(&sin), sizeof sin) == SOCKET_ERROR) {
		perror("unable to bind !");
		exit(errno);
	}

	puts("bind done");

	if(::listen(masterSocket, 32) == SOCKET_ERROR) {
		perror("listen()");
		exit(errno);
	}

	m_compteurs.emplace_back("carotte", 10);
}

void HttpServer::describe() const {
	std::cout << "The server is listening on port " << m_port << std::endl;
}

void HttpServer::listen(std::string url) {
	m_bindHost = std::move(url);
}

[[noreturn]] void HttpServer::start() {
	loadHtmlContent();

	while(true) {
		acceptRequest();
	}
}

void HttpServer::loadHtmlContent() {
	fs::path htmlHeader("header.html");
	fs::path htmlFooter("footer.html");

	std::cout << "Loading html content ..." << std::endl;

	if(!fs::exists(htmlHeader) || !fs::exists(htmlFooter)) {
		//One or more required file are missing
		throw std::runtime_error("One or more required file(s) are missing");
	}

	std::ifstream header(htmlHeader.string());
	if (!header)
		throw std::runtime_error("failed to open header.html");

	std::ifstream footer(htmlFooter.string());
	if (!footer)
		throw std::runtime_error("failed to open header.html");

	m_htmlHeader = std::string((std::istreambuf_iterator<char>(header)), std::istreambuf_iterator<char>());
	m_htmlFooter = std::string((std::istreambuf_iterator<char>(footer)), std::istreambuf_iterator<char>());

	std::cout << "html content loaded !" << std::endl;
}

void HttpServer::acceptRequest() {
	sockaddr_in csin = {0};
	socklen_t sinsize = sizeof csin;
	SOCKET csock_tmp = accept(masterSocket, reinterpret_cast<sockaddr*>(&csin), &sinsize);

	if(csock_tmp != INVALID_SOCKET) {
		//handle client request here
		handleRoute(csock_tmp);
	} else {
		perror("Invalid socket !");
	}
}

void HttpServer::handleRoute(SOCKET client) {
	char buffer[1024];

	recv(client, buffer, 1024, 0);

	Request request(client, buffer);
	request.process();

	Http::Url requestedUrl = request.getUrl();
	Http::Verb method = request.getVerb();

	Response response(client);

	//If the requested host is the same as configured
	if(request.getHeader("Host").find(m_bindHost) != std::string::npos) {
		if(method == Http::HEAD) {
			response.dryRun();
			method = Http::GET;
		}

		if(method == Http::GET) {
			if(requestedUrl == Http::Url::GET_ALL_CPT) {
				//show all counters, formatted
				http_get_all_counters(&request, &response);
			} else {
				http_get_counter(&request, &response);
			}
		} else if(method == Http::POST) {
			http_post_counter(&request, &response);
		} else if(method == Http::PUT) {
			http_put_counter(&request, &response);
		} else if(method == Http::DELETE_VERB) {
			http_del_counter(&request, &response);
		} else {
			//Bad request
			response.setHttpStatusCode(400);
			response.write();
		}
	} else {
		response.setHttpStatusCode(404);
		response.write();
	}

	delay();
	closesocket(client);
}

void HttpServer::respond(Request *request, Response *response, const Compteur& cpt) {
	std::string acceptHeader = request->getHeader("Accept");
	std::stringstream resp;

	response->setHttpStatusCode(200);

	//Determine server response format
	if(Http::RequestUtil::AcceptEverything(acceptHeader)) {
		//We respond html
		response->html();
		resp << m_htmlHeader;
		resp << "<tr><td>" << cpt.getNom() << "</td><td>" << std::to_string(cpt.getVal()) << "</td></tr>";
		resp << m_htmlFooter;
	} else if(Http::RequestUtil::AcceptJson(acceptHeader)) {
		//Some json
		response->json();
		resp << jsonify(cpt);
	} else {
		//Respond text
		response->setContentType("text/plain");
		resp << cpt.getNom() + ": " + std::to_string(cpt.getVal());
	}

	response->write(resp.str());
}

void HttpServer::respond(Request *request, Response *response, const std::vector<Compteur> &counters) {
	std::string acceptHeader = request->getHeader("Accept");
	std::stringstream resp;

	response->setHttpStatusCode(200);

	Compteur cptEtoile = getStarCounter();

	//Determine server response format
	if(Http::RequestUtil::AcceptEverything(acceptHeader)) {
		//Send html
		response->html();
		resp << m_htmlHeader;

		for(const auto &cpt : counters) {
			resp << "<tr><td>" << cpt.getNom() << "</td><td>" << std::to_string(cpt.getVal()) << "</td></tr>";
		}

		resp << "<tr><td>" << cptEtoile.getNom() << "</td><td>" << std::to_string(cptEtoile.getVal()) << "</td></tr>";

		resp << m_htmlFooter;
	} else if(Http::RequestUtil::AcceptJson(acceptHeader)) {
		response->json();
		resp << "{'counters': [";

		for(const auto& cpt : counters) {
			resp << "{'name': '" << cpt.getNom() << "', 'value': '" << cpt.getVal() << "'},";
		}

		resp << "{'name': '" << cptEtoile.getNom() << "', 'value': '" << cptEtoile.getVal() << "'},";

		resp << "]}";
	}

	response->write(resp.str());
}

std::string HttpServer::jsonify(const Compteur& cpt) {
	return "{'compteur': '" + cpt.getNom() + "', 'value': '" + std::to_string(cpt.getVal()) + "'}";
}

void HttpServer::http_get_counter(Request *request, Response *response, const std::string& name) {
	//Get a specific counter
	std::string counter;

	if(name.empty()) {
		counter = request->getCounterName();
	} else {
		counter = name;
	}

	//If this is the special counter
	//Count all val of counters
	if(counter == "etoile") {
		respond(request, response, getStarCounter());
	}
	else if (auto compteurOpt = getCounter(counter))
	{
		respond(request, response, *compteurOpt);
	}
	else
		response->sendNotFound();
}

void HttpServer::http_get_all_counters(Request *request, Response *response) {
	respond(request, response, m_compteurs);
}

void HttpServer::http_post_counter(Request *request, Response *response) {
	std::string name = request->getParam("name");

	//Create a new counter
	if(!name.empty()) {
		//Header exists and are of expected type
		if(!counterExists(name)) { //If the counter doesn't exists, we create it
			Compteur cpt(name, 1);
			m_compteurs.push_back(cpt);
		}

		//Redirect user to other page
		http_get_counter(request, response, name);
	} else {
		//Send an error to the client
		response->setHttpStatusCode(400);
		response->write();
	}
}

void HttpServer::http_put_counter(Request *request, Response *response) {
	std::string counterName = request->getCounterName();
	std::string val = request->getParam("value");

	if(!counterName.empty() && !val.empty()) {
		for(auto &cpt : m_compteurs) {
			if(cpt.getNom() == counterName) {
				int v = std::stoi(val);

				if(v > cpt.getVal()) {
					cpt.setVal(v);

					response->setHttpStatusCode(204);
					response->write("");

					return;
				}

				break;
			}
		}
	}

	//If any error
	response->setHttpStatusCode(400);
	response->write();
}

void HttpServer::http_del_counter(Request *request, Response *response) {
	std::string counterName = request->getCounterName();

	if(!counterName.empty()) {
		for(auto it = m_compteurs.begin(); it != m_compteurs.end(); it++) {
			if(it->getNom() == counterName) {
				m_compteurs.erase(it);

				response->setHttpStatusCode(200);
				response->write("");

				return;
			}
		}
	}

	//If any error
	response->setHttpStatusCode(400);
	response->write();
}

std::optional<std::reference_wrapper<Compteur>> HttpServer::getCounter(const std::string &name) {
	for(auto& cpt : m_compteurs) {
		if(cpt.getNom() == name) {
			return cpt;
		}
	}

	return {};
}

Compteur HttpServer::getStarCounter() {
	Compteur cpt("etoile", 0);

	for(auto &counter : m_compteurs) {
		cpt.inc(counter.getVal());
	}

	return cpt;
}

bool HttpServer::counterExists(const std::string &name) {
	for(const auto& cpt : m_compteurs) {
		if(cpt.getNom() == name) {
			return true;
		}
	}

	return false;
}

void HttpServer::delay() {
	std::cout << "Facto Time !" << std::endl;

	for(int i = 0.; i < 20.; i++) {
		facto(static_cast<double>(i));
	}
}

double HttpServer::facto(double n) {
	if(n > 1.) {
		std::this_thread::sleep_for(std::chrono::milliseconds(5)); //Be nice with the OS and the CPU

		return n * facto(n - 1.) + std::sqrt(n);
	} else {
		return 1;
	}
}
