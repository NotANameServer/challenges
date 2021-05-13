using System;
using System.Collections.Generic;

namespace Elanis.Webserver {
	public class HttpRequest {
		private readonly string rawRequestContent;
		private readonly IList<string> RawHeadersLines;

		public HttpMethod Method { get; private set; }
		public string RequestPath { get; private set; }
		public string HttpVersion { get; private set; }

		public Dictionary<string, string> Headers { get; private set; } = new Dictionary<string, string>();
		public string Body { get; private set; }

		public HttpRequest(string rawRequestContent) {
			this.rawRequestContent = rawRequestContent.Replace("\r\n", "\n");


			var headerEnd = this.rawRequestContent.IndexOf("\n\n");
			this.RawHeadersLines = this.rawRequestContent.Substring(0, headerEnd).Split("\n");
			this.Body = this.rawRequestContent[(headerEnd + 2)..];

			this.ParseRequest();
		}

		private void ParseHeaders() {
			foreach (string line in RawHeadersLines) {
				int colonIndex = line.IndexOf(":");

				if (colonIndex > 0) {
					string headerName = line.Substring(0, colonIndex);
					string headerValue = line[(colonIndex + 1)..].Trim();

					this.Headers[headerName] = headerValue;
				}
			}
		}

		private void ParseRequest() {
			this.ParseHeaders();

			string[] firstLineWords = RawHeadersLines[0].Split(' ');

			try {
				this.Method = Enum.Parse<HttpMethod>(firstLineWords[0]);
			} catch (Exception) {
				throw new ArgumentOutOfRangeException("Invalid Http Method");
			}
			this.RequestPath = firstLineWords[1];
			this.HttpVersion = firstLineWords[2];
		}
	}
}
