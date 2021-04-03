using System;
using System.Net;

namespace Elanis.Webserver {
	class Program {
		private static readonly IPAddress listeningAddress = IPAddress.Parse("127.0.0.1");
		private static readonly int port = 8080;

		public static void Main() {
			var httpServer = new HttpServer(listeningAddress, port);

			httpServer.Listen();

			Console.WriteLine("\nHit enter to continue...");
			Console.Read();
		}
	}
}
