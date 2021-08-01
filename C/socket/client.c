#include <stdio.h>
#include <string.h>
#include <stddef.h>

#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#include "common.h"


int main(const int argc, const char *const argv[])
{
	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 3 || parse_address(argv[1], argv[2], &server) != 0) {
		printf("Usage: %s <address> <port>\n", argv[0]);
		return -1;
	}

	// establish connection
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	int err = connect(channel, (struct sockaddr *)&server, sizeof(server));
	if (err) {
		printf("Error: Couldn't connect to server.\n");
		return channel;
	}
	printf("Successfully connected to server, press Ctrl-D to exit.\n");

	for (char line[128];;) {
		// get user input
		memset(line, 0, sizeof(line));
		if (!fgets(line, sizeof(line), stdin) || strcmp(line, "sair\n") == 0) {
			break;
		}

		// validate expression
		char op;
		float lhs, rhs;
		int match = sscanf(line, " %f %1[+-*/] %f ", &lhs, &op, &rhs);
		if (match != 3) {
			printf("Error: Wrong syntax on input '%s', try again.\n", line);
			continue;
		}

		// send request
		const int size = snprintf(line, sizeof(line), "(%c %g %g)\0", op, lhs, rhs);
		const ssize_t sent = send(channel, line, size, 0);
		if (sent < 0) {
			printf("Error: Couldn't send request on socket %d.\n", channel);
			close(channel);
			return sent;
		}

		// receive response
		ssize_t received = recv(channel, line, sizeof(line), 0);
		if (received <= 0) {
			printf("Error: Couldn't receive response from the server.\n");
			close(channel);
			return received;
		}

		// clean unused part of the buffer, then parse it
		received = received >= sizeof(line) ? sizeof(line) : received;
		memset(line + received, 0, sizeof(line) - received);
		float answer;
		match = sscanf(line, " %f ", &answer);
		if (match != 1) {
			printf("Error: Received invalid response '%s'.\n", line);
			close(channel);
			return -2;
		}

		// show output
		printf("%g\n", answer);
	}

	// exit gracefully
	shutdown(channel, SHUT_WR);
	close(channel);
	return 0;
}
