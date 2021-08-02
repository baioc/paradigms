#include <stdio.h>

#include <sys/socket.h> // socket, connect
#include <netinet/in.h> // sockaddr_in
#include <unistd.h> // close

#include "shared.h"


int main(const int argc, const char *const argv[])
{
	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 4 || parse_address(argv[1], argv[2], &server) != 0) {
		fprintf(stderr, "Usage: %s <server_address> <server_port> <username>\n", argv[0]);
		return -1;
	}
	const char *const username = argv[3];

	// create a communication channel to be used throughout the program
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	if (channel < 0) {
		fprintf(stderr, "Error: Failed to open a new socket\n");
		return channel;
	}

	// establish a connection to the server
	const int err = connect(channel, (struct sockaddr *)&server, sizeof(server));
	if (err) {
		fprintf(stderr, "Error: Couldn't connect to server at %s:%s\n", argv[1], argv[2]);
		return err;
	}
	printf("[INFO] Successfully connected to the server.\n");

	// for (char line[128];;) {
	// 	// get user input
	// 	memset(line, 0, sizeof(line));
	// 	if (!fgets(line, sizeof(line), stdin) || strcmp(line, "sair\n") == 0) {
	// 		break;
	// 	}

	// 	// validate expression
	// 	char op;
	// 	float lhs, rhs;
	// 	int match = sscanf(line, " %f %1[+-*/] %f ", &lhs, &op, &rhs);
	// 	if (match != 3) {
	// 		printf("Error: Wrong syntax on input '%s', try again.\n", line);
	// 		continue;
	// 	}

	// 	// send request
	// 	const int size = snprintf(line, sizeof(line), "(%c %g %g)\0", op, lhs, rhs);
	// 	const ssize_t sent = send(channel, line, size, 0);
	// 	if (sent < 0) {
	// 		printf("Error: Couldn't send request on socket %d.\n", channel);
	// 		close(channel);
	// 		return sent;
	// 	}

	// 	// receive response
	// 	ssize_t received = recv(channel, line, sizeof(line), 0);
	// 	if (received <= 0) {
	// 		printf("Error: Couldn't receive response from the server.\n");
	// 		close(channel);
	// 		return received;
	// 	}

	// 	// clean unused part of the buffer, then parse it
	// 	received = received >= sizeof(line) ? sizeof(line) : received;
	// 	memset(line + received, 0, sizeof(line) - received);
	// 	float answer;
	// 	match = sscanf(line, " %f ", &answer);
	// 	if (match != 1) {
	// 		printf("Error: Received invalid response '%s'.\n", line);
	// 		close(channel);
	// 		return -2;
	// 	}

	// 	// show output
	// 	printf("%g\n", answer);
	// }

	// exit gracefully
	close(channel);
	return 0;
}
