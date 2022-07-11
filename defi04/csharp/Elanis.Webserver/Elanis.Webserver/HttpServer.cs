﻿using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;

namespace Elanis.Webserver {
	class HttpServer {
		private readonly IPAddress listeningAddress;
		private readonly int port;

		public HttpServer(IPAddress listeningAddress, int port) {
			this.listeningAddress = listeningAddress;
			this.port = port;
		}

		public void Listen() {
			// Based on Microsoft documentation at https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.tcplistener

			TcpListener server = null;
			try {
				// TcpListener server = new TcpListener(port);
				server = new TcpListener(listeningAddress, port);

				// Start listening for client requests.
				server.Start();

				// Enter the listening loop.
				while (true) {
					Console.Write("Waiting for a connection... ");

					// Perform a blocking call to accept requests.
					TcpClient client = server.AcceptTcpClient();

					Task.Run(() => {
						HandleRequest(client);
					});
				}
			} catch (SocketException e) {
				Console.WriteLine("SocketException: {0}", e);
			} finally {
				// Stop listening for new clients.
				server.Stop();
			}
		}

		private void HandleRequest(TcpClient client) {
			try {
				// Get Data
				NetworkStream stream = client.GetStream();
				byte[] bytes = new byte[4096];
				int size = stream.Read(bytes, 0, bytes.Length);

				// Work on data
				byte[] msg = YaGotHttpPayloadToWorkOn(bytes, size);

				// Reply
				stream.Write(msg, 0, msg.Length);

			} catch (Exception e) {
				Console.WriteLine("Exception: {0}", e);
			} finally {
				// Shutdown and end connection
				client.Close();
			}
		}

		private byte[] YaGotHttpPayloadToWorkOn(byte[] bytes, int size) {
			string data = System.Text.Encoding.ASCII.GetString(bytes, 0, size);
			Console.WriteLine("Received: {0}", data);

			var request = new HttpRequest(data);

			// TODO: understand request and do work

			var response = new HttpResponse(HttpStatusCode.OK, "<P>Ceci est une page d'exemple.</P>", "text/html");

			data = response.ToString();
			Console.WriteLine("Sent: {0}", data);

			byte[] msg = System.Text.Encoding.ASCII.GetBytes(data);

			return msg;
		}
	}
}
