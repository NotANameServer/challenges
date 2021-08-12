//
// Created by Romain Neil on 28/03/2021.
//

#pragma once

#ifndef HTTP_COUNTER_UTILS_H
#define HTTP_COUNTER_UTILS_H

#include <iostream>
#include <string>
#include "http_status_codes.h"
#include "Compteur.h"

namespace Http {

	/**
	 * HTTP 1.1 Verb, take in account
	 */
	enum Verb {
		GET = 0,
		HEAD,
		POST,
		PUT,
		DELETE_VERB,

		BAD_METHOD //Keep last
	};

	/**
	 * Liste des différentes routes
	 */
	enum Url {
		UNKNOWN = 1,

		GET_ALL_CPT,
		GET_SPECIFIC_CPT,

		POST_CREATE_CPT,

		PUT_EDIT_CPT,

		DELETE_CPT
	};

	namespace RequestUtil {

		inline bool AcceptEverything(const std::string &acceptHeader) {
			return (acceptHeader.find("text/html") != std::string::npos) || (acceptHeader.find("*/*") != std::string::npos);
		}

		inline bool AcceptJson(const std::string &acceptHeader) {
			return (acceptHeader.find("application/json") != std::string::npos);
		}

	}

}

#if defined(_WIN32)

#include <winsock2.h>
#include <WS2tcpip.h>

#pragma comment(lib, "ws2_32")

#else

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>

#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#define closesocket(s) close(s)

typedef int SOCKET;
typedef struct sockaddr_in SOCKADDR_IN;
typedef struct sockaddr SOCKADDR;
typedef struct in_addr IN_ADDR;

#endif

#endif //HTTP_COUNTER_UTILS_H
