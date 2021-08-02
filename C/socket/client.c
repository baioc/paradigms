#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>

#include <sys/socket.h> // socket, connect, send, recv, bind, listen, accept, getsockopt, setsockopt
#include <netinet/in.h> // sockaddr_in,
#include <unistd.h> // close
#include <arpa/inet.h> // ntohs, inet_ntoa
#include <unistd.h> // sleep

#include "shared.h"


static char buffer[MAX_MESSAGE_SIZE];


// plays a tic-tac-toe game against a remote player
void play(
	board_t game_state,
	bool my_turn,
	const char *const me,
	const char *const they,
	const int channel,
	const char *const player1,
	const char *const player2,
	const char *const server_address,
	const char *const server_port,
	const token_t match,
	const token_t my_secret,
	token_t signature
) {
	// (P <match> <board> <signature>)
	if (my_turn) {
		printf("I'm playing\n");
	}
	else {
		printf("I'm not\n");
	}
}


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
	const char* server_address = argv[1];
	const char* server_port = argv[2];

	// create a communication channel to be used throughout the program
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	bool reuse_addr = true;
	setsockopt(channel, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, 1);
	if (channel < 0) {
		eprintf("# Failed to open a new socket\n");
		return errno;
	}

	// establish a connection with the server
	const int err = connect(channel, (struct sockaddr *)&server, sizeof(server));
	if (err) {
		eprintf("# Couldn't connect to server at %s:%s\n", server_address, server_port);
		return errno;
	}
	eprintf("# Reached server at %s:%s\n", server_address, server_port);

	if (argc == 4) {
		// get the scoreboard from the server
		const int size = snprintf(buffer, sizeof(buffer), "(G)\0");
		const ssize_t sent = send(channel, buffer, size, 0);
		if (sent <= 0) {
			eprintf("# Couldn't send request\n");
			close(channel);
			return errno;
		}

		// receive and print response
		ssize_t received = recv(channel, buffer, sizeof(buffer), 0);
		if (received <= 0) {
			eprintf("# Couldn't fetch score board\n");
			close(channel);
			return errno;
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
			eprintf("# Couldn't send request\n");
			close(channel);
			return errno;
		}

		// receive response and clean unused part of the buffer
		ssize_t received = recv(channel, buffer, sizeof(buffer), 0);
		if (received <= 0) {
			eprintf("# Couldn't connect as %s\n", username);
			close(channel);
			return errno;
		}
		received = received >= sizeof(buffer) ? sizeof(buffer) : received;
		memset(buffer + received, 0, sizeof(buffer) - received);

		// whatever happens next, we're done with the server connection
		struct sockaddr_in peer;
		socklen_t peer_len = sizeof(peer);
		getsockname(channel, (struct sockaddr *)&peer, &peer_len);
		shutdown(channel, SHUT_RDWR);
		close(channel);

		// try parsing a "wait for turn" response
		char bw;
		token_t match;
		char opponent_name[MAX_USERNAME_SIZE];
		int matches = sscanf(buffer, " ( %1[W] %u %[^)]s ) ", &bw, &match, opponent_name);
		if (matches == 3) {
			// open a new socket for this P2P match, MUST reuse previous port
			const int channel = socket(AF_INET, SOCK_STREAM, 0);
			bool reuse_addr = true;
			setsockopt(channel, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, 1);
			int err = 0;
			err = bind(channel, (struct sockaddr *)&peer, peer_len);
			err = listen(channel, 1);
			if (err) {
				eprintf("# Couldn't reacquire port %u for P2P match\n", ntohs(peer.sin_port));
				close(channel);
				return errno;
			}
			printf("# Waiting for player %s to join local match at %s:%u\n",
			       opponent_name, inet_ntoa(peer.sin_addr), ntohs(peer.sin_port));
			struct sockaddr_in opponent;
			socklen_t opponent_len = sizeof(opponent);
			const int connection = accept(channel, (struct sockaddr *)&opponent, &opponent_len);
			if (connection < 0) {
				eprintf("# Couldn't accept incoming connection\n");
				close(connection);
				return errno;
			}
			eprintf("# Incoming connection from %s:%d\n",
			        inet_ntoa(opponent.sin_addr), ntohs(opponent.sin_port));
			close(channel); // no need to listen for any other players
			play(EMPTY_BOARD, false, username, opponent_name,
			     connection, opponent_name, username, server_address, server_port,
			     match, password, INITIAL_MODULUS);
			close(connection);
			goto EXIT_OK;
		}

		// try parsing a "begin playing" response
		char address[128];
		char port[16];
		matches = sscanf(buffer, " ( %1[B] %u %s %s %[^)]s ) ",
		                 &bw, &match, opponent_name, address, port);
		if (matches == 5 && bw == 'B' && parse_address(address, port, &peer) == 0) {
			// connect to peer after giving him some time to set things up
			printf("# Waiting for player %s to set up a match", opponent_name); fflush(stdout);
			sleep(1); printf("."); fflush(stdout);
			sleep(1); printf("."); fflush(stdout);
			sleep(1); printf(".\n"); fflush(stdout);
			const int channel = socket(AF_INET, SOCK_STREAM, 0);
			const int err = connect(channel, (struct sockaddr *)&peer, sizeof(peer));
			if (err) {
				eprintf("# Couldn't connect to player %s at %s:%s\n", opponent_name, address, port);
				return errno;
			}
			eprintf("# Reached player %s at %s:%s\n", opponent_name, address, port);
			play(EMPTY_BOARD, true, username, opponent_name,
			     channel, username, opponent_name, server_address, server_port,
			     match, password, INITIAL_MODULUS);
			close(channel);
			goto EXIT_OK;
		}

		eprintf("# Quitting after invalid message: %.*s\n", received, buffer);
		return channel;
	}

	// exit gracefully
EXIT_OK:
	close(channel);
	return 0;
}
