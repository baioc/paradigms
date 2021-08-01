#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "common.h"


int main(const int argc, const char *const argv[])
{
	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 3 || parse_address(argv[1], argv[2], &server) != 0) {
		printf("Usage: %s <address> <port>\n", argv[0]);
		return -1;
	}

	// start listening
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	int err = 0;
	err = bind(channel, (struct sockaddr *)&server, sizeof(server));
	err = listen(channel, 5);
	if (err) {
		printf("Error: Failed to listen on socket %d.\n", channel);
		return err;
	}

	for (;;) {
		// wait for a client to connect
		printf("Waiting for client, press Ctrl-C to quit.\n");
		struct sockaddr_in client;
		socklen_t client_len = sizeof(client);
		const int connection = accept(channel, (struct sockaddr *)&client, &client_len);
		if (connection < 0) {
			printf("Error: Couldn't establish incoming connection.\n");
			return connection;
		}
		printf("Incoming connection from %s:%d\n",
		       inet_ntoa(client.sin_addr), ntohs(client.sin_port));

		for (char buf[128];;) {
			// wait for request
			memset(buf, 0, sizeof(buf));
			const ssize_t received = recv(connection, buf, sizeof(buf), 0);
			if (received == 0) {
				printf("Client went down, closing connection.\n");
				break;
			} else if (received < 0) {
				printf("Error: Couldn't receive request from the client.\n");
				break;
			}

			// parse request
			char op;
			float lhs, rhs;
			const int match = sscanf(buf, " ( %1[+-*/] %f %f ) ", &op, &lhs, &rhs);
			if (match != 3) {
				printf("Error: Invalid message format, dropping connection %d.", connection);
				break;
			}

			// evaluate request
			float answer;
			switch (op) {
			case '+': answer = lhs + rhs; break;
			case '-': answer = lhs - rhs; break;
			case '*': answer = lhs * rhs; break;
			case '/': answer = lhs / rhs; break;
			default: assert(0); // unreachable
			}
			printf("%s => %g\n", buf, answer);

			// send response
			const int size = snprintf(buf, sizeof(buf), "%g\0", answer);
			const ssize_t sent = send(connection, buf, size, 0);
			if (sent < 0) {
				printf("Error: Couldn't send reply on socket %d.\n", connection);
				break;
			}
		}

		// close connection to this client
		shutdown(connection, SHUT_RD);
		close(connection);
	}

	// exit gracefully
	shutdown(channel, SHUT_RDWR);
	close(channel);
	return 0;
}
