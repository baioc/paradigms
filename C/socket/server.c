#include <stdio.h>
#include <stddef.h>

#include <sys/socket.h> // socket, connect, bind, listen, accept
#include <netinet/in.h> // sockaddr_in
#include <arpa/inet.h> // inet_ntoa, ntohs
#include <unistd.h> // close

#include "shared.h"


int main(const int argc, const char *const argv[])
{
	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 3 || parse_address(argv[1], argv[2], &server) != 0) {
		fprintf(stderr, "Usage: %s <address> <port>\n", argv[0]);
		return -1;
	}

	// start listening for clients
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	int err = 0;
	err = bind(channel, (struct sockaddr *)&server, sizeof(server));
	err = listen(channel, 30);
	if (err) {
		fprintf(stderr, "Error: Failed to listen on %s:%s through socket %d\n", argv[1], argv[2], channel);
		return err;
	}

	for (;;) {
		// wait for a client to connect
		printf("[INFO] Waiting for client(s), press Ctrl-C to quit.\n");
		struct sockaddr_in client;
		socklen_t client_len = sizeof(client);
		const int connection = accept(channel, (struct sockaddr *)&client, &client_len);
		if (connection < 0) {
			printf("[ERROR] Couldn't accept incoming connection.\n");
			continue;
		}
		printf("[INFO] Incoming connection from %s:%d.\n", inet_ntoa(client.sin_addr), ntohs(client.sin_port));

	// 	for (char buf[128];;) {
	// 		// wait for request
	// 		memset(buf, 0, sizeof(buf));
	// 		const ssize_t received = recv(connection, buf, sizeof(buf), 0);
	// 		if (received == 0) {
	// 			printf("Client went down, closing connection.\n");
	// 			break;
	// 		} else if (received < 0) {
	// 			printf("Error: Couldn't receive request from the client.\n");
	// 			break;
	// 		}

	// 		// parse request
	// 		char op;
	// 		float lhs, rhs;
	// 		const int match = sscanf(buf, " ( %1[+-*/] %f %f ) ", &op, &lhs, &rhs);
	// 		if (match != 3) {
	// 			printf("Error: Invalid message format, dropping connection %d.", connection);
	// 			break;
	// 		}

	// 		// evaluate request
	// 		float answer;
	// 		switch (op) {
	// 		case '+': answer = lhs + rhs; break;
	// 		case '-': answer = lhs - rhs; break;
	// 		case '*': answer = lhs * rhs; break;
	// 		case '/': answer = lhs / rhs; break;
	// 		default: assert(0); // unreachable
	// 		}
	// 		printf("%s => %g\n", buf, answer);

	// 		// send response
	// 		const int size = snprintf(buf, sizeof(buf), "%g\0", answer);
	// 		const ssize_t sent = send(connection, buf, size, 0);
	// 		if (sent < 0) {
	// 			printf("Error: Couldn't send reply on socket %d.\n", connection);
	// 			break;
	// 		}
	// 	}

		// close connection to this client
		close(connection);
	}

	// exit gracefully
	close(channel);
	return 0;
}
