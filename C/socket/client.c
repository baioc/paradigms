#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h> // socket, connect
#include <netinet/in.h> // sockaddr_in
#include <unistd.h> // close

#include "shared.h"


static char buffer[MAX_MESSAGE_SIZE];


int main(const int argc, const char *const argv[])
{
	// validate CLI usage
	struct sockaddr_in server;
	if (argc < 4
	    || parse_address(argv[1], argv[2], &server) != 0
	    || (argc == 4 && strcmp(argv[3], "--scoreboard") != 0)
	    || (argc == 5 && (strlen(argv[3]) >= MAX_USERNAME_SIZE || atoi(argv[4]) <= 0))
	    || argc > 5
	) {
		eprintf("Usage: %s <server_address> <server_port>"
		        "(--scoreboard | <username> <numeric_password>)\n",
		        argv[0]);
		return 1;
	}

	// create a communication channel to be used throughout the program
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	if (channel < 0) {
		eprintf("Failed to open a new socket\n");
		return channel;
	}

	// establish a connection with the server
	const int err = connect(channel, (struct sockaddr *)&server, sizeof(server));
	if (err) {
		eprintf("Couldn't connect to server at %s:%s\n", argv[1], argv[2]);
		return err;
	}
	eprintf("Reached server at %s:%s\n", argv[1], argv[2]);

	if (argc == 4) {
		// get the scoreboard from the server
		const int size = snprintf(buffer, sizeof(buffer), "(G)\0");
		const ssize_t sent = send(channel, buffer, size, 0);
		if (sent < 0) {
			eprintf("Couldn't send request\n");
			close(channel);
			return sent;
		}

		// receive and print response
		ssize_t received = recv(channel, buffer, sizeof(buffer), 0);
		if (received <= 0) {
			eprintf("Couldn't fetch score board\n");
			close(channel);
			return channel;
		}
		printf("%.*s", received, buffer);
	}
	else if (argc == 5) {
		// try to register / log in
		const char *const username = argv[3];
		token_t password = atoi(argv[4]);
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
		printf("%s\n", buffer); // TODO: now play it
	}

	// exit gracefully
	close(channel);
	return 0;
}
