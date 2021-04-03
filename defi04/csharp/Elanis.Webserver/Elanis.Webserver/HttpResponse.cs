using System;
using System.Net;

namespace Elanis.Webserver {
	public class HttpResponse {
		private const string SERVER_NAME = "Apache... Nan jdeconne, Elanis Web Server/1.0.0";

		private readonly string statusText;
		private readonly string content;
		private readonly string contentType;
		private readonly DateTime date;

		public HttpResponse(HttpStatusCode statusCode, string content, string contentType) {
			this.statusText = (int)statusCode + " " + statusCode.ToString();

			this.date = DateTime.Now;

			this.content = content;
			this.contentType = contentType;
		}

		public override string ToString() {
			return string.Format(
@"HTTP/1.0 {0}
Date: {1}
Server: Apache... Nan jdeconne, Elanis Web Server/1.0.0
Content-Type: {2}
Content-Length: {3}
Expires: {1}
Last-modified: {1}
Access-Control-Allow-Origin: *

{4}"
	, statusText, date.ToString("r"), contentType, content.Length, content
);
		}
	}
}
