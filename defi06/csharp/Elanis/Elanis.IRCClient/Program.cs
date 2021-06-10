using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;

namespace Elanis.IRCClient {
	class Program {
		private const string serverAddr = "drlazor.be";
		private const int serverPort = 6667;
		private const string nickname = "Elanis";
		static string lastChannel = "";

		static void WaitUserInputMsg(Socket sender) {
			Console.ForegroundColor = ConsoleColor.White;
			Console.Write("\r>: ");
		}

		static void Send(Socket sender, string msg) {
			Console.ForegroundColor = ConsoleColor.DarkGray;
			Console.WriteLine("\r[SOCKET SEND] " + msg);

			WaitUserInputMsg(sender);


			sender.Send(Encoding.UTF8.GetBytes(msg + "\r\n"));
		}

		static void Answer(Socket sender, string data) {
			if (data == "PING drlazor.be\r\n") {
				Send(sender, "PONG drlazor.be");

				Console.ForegroundColor = ConsoleColor.DarkBlue;
				Console.WriteLine("PING");
			} else if (data.StartsWith(":")) {

				Console.ForegroundColor = ConsoleColor.White;
				Console.WriteLine("\r" + data);
			} else {

				Console.ForegroundColor = ConsoleColor.DarkGray;
				Console.WriteLine("\r[SOCKET RECIEVE] " + data);
			}

			WaitUserInputMsg(sender);
		}

		static void LoopRecieve(Socket sender) {
			byte[] bytes = new byte[1024]; // Data buffer for incoming data.  
			while (true) {
				// Receive the response from the remote device.  
				int bytesRec = sender.Receive(bytes);

				var strData = Encoding.UTF8.GetString(bytes, 0, bytesRec);
				if (strData == "") {
					continue;
				}

				if (strData.Contains("PRIVMSG " + lastChannel)) {
					Console.WriteLine(strData.Replace(" PRIVMSG", ""));
					return;
				}

				Answer(sender, strData);
			}
		}

		static void LoopSend(Socket sender) {
			string str;
			byte[] bytes = new byte[1024]; // Data buffer for incoming data.  

			Send(sender, "NICK Elanis");
			Send(sender, "USER Elanis 0 * :Elanis");

			while (true) {
				WaitUserInputMsg(sender);
				str = Console.ReadLine();

				if (str == ":q") {
					break;
				}

				str = str.Replace(":shrug:", "¯\\_(ツ)_/¯");
				str = str.Replace(":tableflip:", "(╯°□°）╯︵ ┻━┻");

				if (str.StartsWith(":join ")) {
					string[] parts = str.Split(" ");
					lastChannel = parts[1];
					str = "JOIN " + lastChannel;
				} else if (str.StartsWith(":")) {
					str = str.Substring(1);
				} else {
					str = "PRIVMSG " + lastChannel + " :" + str;
				}

				Send(sender, str);
			}

			sender.Send(Encoding.UTF8.GetBytes("QUIT :Bye !"));
		}

		public static void StartClient() {
			// Connect to a remote device.  
			try {
				// Establish the remote endpoint for the socket.  
				// This example uses port 11000 on the local computer.  
				IPHostEntry ipHostInfo = Dns.GetHostEntry(serverAddr);
				IPAddress ipAddress = ipHostInfo.AddressList[0];
				IPEndPoint remoteEP = new IPEndPoint(ipAddress, serverPort);

				// Create a TCP/IP  socket.  
				Socket sender = new Socket(ipAddress.AddressFamily,
					SocketType.Stream, ProtocolType.Tcp);

				// Connect the socket to the remote endpoint. Catch any errors.  
				try {
					sender.Connect(remoteEP);

					Console.WriteLine("Socket connected to {0}",
						sender.RemoteEndPoint.ToString());

					Task.Run(() => LoopRecieve(sender));
					LoopSend(sender);

					// Release the socket.  
					sender.Shutdown(SocketShutdown.Both);
					sender.Close();

				} catch (ArgumentNullException ane) {
					Console.WriteLine("ArgumentNullException : {0}", ane.ToString());
				} catch (SocketException se) {
					Console.WriteLine("SocketException : {0}", se.ToString());
				} catch (Exception e) {
					Console.WriteLine("Unexpected exception : {0}", e.ToString());
				}

			} catch (Exception e) {
				Console.WriteLine(e.ToString());
			}
		}


		static int Main(string[] args) {
			StartClient();
			return 0;
		}
	}
}
