//
// Created by Romain Neil on 28/03/2021.
//

#include "Response.h"

Response::Response(SOCKET socket) : m_socket(socket) {
	m_response << "HTTP/1.1 ";
}

void Response::write() {
	setContentType("text/plain");
	write("Error !");
}

void Response::sendNotFound() {
	setHttpStatusCode(404);
	setContentType("text/html");
	write("<p>The page you are looking for are not found</p>");
}

void Response::dryRun() {
	m_isDryRun = true;
}

void Response::write(const std::string& data) {
	setHeader("Server", "TinyWebServer v1.0");
	setHeader("Content-Length", std::to_string(data.length()));

	if(!m_isDryRun) {
		m_response << std::endl << data;
	}

	//Write content to socket
	send(m_socket, m_response.str().c_str(), m_response.str().length(), 0);
}

void Response::setContentType(const std::string &contentType) {
	setHeader("Content-Type", contentType);
}

void Response::setHeader(const std::string& headerName, const std::string& headerValue) {
	m_response << headerName << ": " << headerValue << std::endl;
}

void Response::setHttpStatusCode(int code) {
	m_response << code << " " << HttpStatus::reasonPhrase(code) << std::endl;
}

void Response::html() {
	setContentType("text/html; charset=utf-8");
}

void Response::json() {
	setContentType("application/json");
}
