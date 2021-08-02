#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h> // socket, connect
#include <netinet/in.h> // sockaddr_in
#include <unistd.h> // close

#include "shared.h"


int main(const int argc, const char *const argv[])
{
	char buffer[MAX_MESSAGE_SIZE];

	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 5
	    || parse_address(argv[1], argv[2], &server) != 0
	    || strlen(argv[3]) >= MAX_USERNAME_SIZE
	    || atoi(argv[4]) <= 0
	) {
		eprintf("Usage: %s <server_address> <server_port> <username> <numeric_password>\n", argv[0]);
		return 1;
	}
	const char *const username = argv[3];
	token_t password = atoi(argv[4]);

	// create a communication channel to be used throughout the program
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	if (channel < 0) {
		eprintf("Failed to open a new socket\n");
		return channel;
	}

	// establish a connection to the server
	const int err = connect(channel, (struct sockaddr *)&server, sizeof(server));
	if (err) {
		eprintf("Couldn't connect to server at %s:%s\n", argv[1], argv[2]);
		return err;
	}
	eprintf("Reached server at %s:%s\n", argv[1], argv[2]);

	// try to register / log in
	const int size = snprintf(buffer, sizeof(buffer), "(N %s %u)\0", username, password);
	const ssize_t sent = send(channel, buffer, size, 0);
	if (sent < 0) {
		eprintf("Couldn't send request\n");
		close(channel);
		return sent;
	}

	// receive response
	ssize_t received = recv(channel, buffer, sizeof(buffer), 0);
	if (received <= 0) {
		eprintf("Couldn't connect as %s\n", username);
		close(channel);
		return channel;
	}

	// clean unused part of the buffer, then parse it
	received = received >= sizeof(buffer) ? sizeof(buffer) : received;
	memset(buffer + received, 0, sizeof(buffer) - received);
	printf("%s\n", buffer);

	// 	// clean unused part of the buffer, then parse it
	// 	received = received >= sizeof(line) ? sizeof(line) : received;
	// 	memset(line + received, 0, sizeof(line) - received);
	// 	float answer;
	// 	match = sscanf(line, " %f ", &answer);
	// 	if (match != 1) {
	// 		printf("Error: Received invalid response '%s'\n", line);
	// 		close(channel);
	// 		return -2;
	// 	}

	// exit gracefully
	close(channel);
	return 0;
}
