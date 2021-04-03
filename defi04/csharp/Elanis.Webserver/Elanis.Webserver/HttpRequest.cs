using System;
using System.Collections.Generic;
using System.Linq;

namespace Elanis.Webserver {
	public class HttpRequest {
		private readonly string rawRequestContent;
		private readonly IList<string> requestLines;

		public HttpMethod Method { get; set; }
		public string RequestPath { get; set; }
		public string HttpVersion { get; set; }

		public string Host { get; set; }
		public string Referer { get; set; }
		public string UserAgent { get; set; }

		public HttpRequest(string rawRequestContent) {
			this.rawRequestContent = rawRequestContent.Replace("\r\n", "\n");
			this.requestLines = this.rawRequestContent.Split("\n");

			this.ParseRequest();
		}

		private string ExtractHeaderValue(string headerName) {
			string line = this.requestLines.FirstOrDefault((string line) => {
				return line.StartsWith(headerName);
			});

			if (string.IsNullOrWhiteSpace(line)) {
				return null;
			}

			return line.Substring(line.IndexOf(":") + 1).Trim();
		}

		private void ParseRequest() {
			string[] firstLineWords = requestLines[0].Split(' ');

			try {
				this.Method = Enum.Parse<HttpMethod>(firstLineWords[0]);
			} catch (Exception) {
				throw new ArgumentOutOfRangeException("Invalid Http Method");
			}
			this.RequestPath = firstLineWords[1];
			this.HttpVersion = firstLineWords[2];

			this.Host = ExtractHeaderValue("Host");
			this.Referer = ExtractHeaderValue("Referer");
			this.UserAgent = ExtractHeaderValue("User-Agent");

		}
	}
}
