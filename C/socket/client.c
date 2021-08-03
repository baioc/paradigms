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


char stoc(const struct Board *board, unsigned position)
{
	switch (board->slots[position]) {
	case BOARD_EMPTY: return position + '0';
	case BOARD_PLAYER_1: return 'X';
	case BOARD_PLAYER_2: return 'O';
	default: return '?';
	}
}

static int board_print(const struct Board *board)
{
	return printf(
		"#  %c | %c | %c \n"
		"# ---|---|---\n"
		"#  %c | %c | %c \n"
		"# ---|---|---\n"
		"#  %c | %c | %c \n",
		stoc(board, 1), stoc(board, 2), stoc(board, 3),
		stoc(board, 4), stoc(board, 5), stoc(board, 6),
		stoc(board, 7), stoc(board, 8), stoc(board, 9)
	);
}

static unsigned make_move(unsigned turn, struct Board *board, Log *log)
{
	board_print(board);
	enum Slot player = turn % 2 == 0 ? BOARD_PLAYER_1 : BOARD_PLAYER_2;
	printf("# Playing as %c, please enter your next move (1-9)\n", player);
	int err;
	unsigned move;
	do {
		const int matches = scanf("%d", &move);
		if (matches != 1) { int c; while ((c = fgetc(stdin)) != '\n' && c != EOF); } // flush stdin
		err = board_play(turn, player, move, board, log);
		if (err) printf("# Invalid move, try again\n");
	} while (err);
	board_print(board);
	return move;
}

struct Match {
	const char *me;
	const char *they;
	const char *player1;
	const char *player2;
	int channel;
	const char *server_address;
	const char *server_port;
	Token match;
	Token my_secret;
};

static void print_result(enum GameStatus status, const struct Match *config)
{
	if (status == GAME_TIE) {
		printf("# Game over: it's a tie!\n");
		return;
	}

	if ((status == GAME_PLAYER_1 && strcmp(config->player1, config->me) == 0)
	 || (status == GAME_PLAYER_2 && strcmp(config->player2, config->me) == 0)
	) {
		printf("# Game over: you win!\n");
	} else {
		printf("# Game over: you lose!\n");
	}
}

/// Plays a tic-tac-toe game against a remote player.
static void play(
	const struct Match *config,
	struct Board *board,
	bool my_turn,
	unsigned turns,
	Log log,
	Log certificate
) {
	bool game_over = false;
	if (my_turn) {
		// make a move, then sign it
		const unsigned move = make_move(turns, board, &log);
		certificate = log_add(certificate, turns, move);
		certificate = log_sign(certificate, config->my_secret);

		// check for game over
		const enum GameStatus status = board_check(board);
		if (status != GAME_NOT_OVER) {
			// display result to player
			print_result(status, config);

			// report it to the server
			struct sockaddr_in server;
			parse_address(config->server_address, config->server_port, &server);
			const int channel = socket(AF_INET, SOCK_STREAM, 0);
			const int err = connect(channel, (struct sockaddr *)&server, sizeof(server));
			const int size = snprintf(buffer, sizeof(buffer), "(E %s %s %lu %lu)\0",
			                          config->player1, config->player2, log, certificate);
			const ssize_t sent = send(channel, buffer, size, 0);
			if (err || sent <= 0) {
				eprintf("# Couldn't reconnect to server\n");
				close(channel);
				close(config->channel);
				exit(errno);
			}
			eprintf("# Sent game result to the server\n");
			close(channel);
			game_over = true;
		}

		// send move to peer
		const int size = snprintf(buffer, sizeof(buffer), "(P %lu %lu %lu)\0",
		                          config->match, log, certificate);
		const ssize_t sent = send(config->channel, buffer, size, 0);
		if (sent <= 0 && !game_over) {
			eprintf("# Lost connection to the match\n");
			close(config->channel);
			exit(errno);
		}

	}
	else {
		// wait for opponent's move
		printf("# Waiting for player %s to make their move...\n", config->they);
		ssize_t received = recv(config->channel, buffer, sizeof(buffer), 0);
		if (received <= 0) {
			eprintf("# Lost connection to the match\n");
			close(config->channel);
			exit(errno);
		}
		received = received >= sizeof(buffer) ? sizeof(buffer) : received;
		memset(buffer + received, 0, sizeof(buffer) - received);

		// validate it
		char p;
		Token match;
		Log new_log;
		Log new_certificate;
		const int matches = sscanf(buffer, " ( %1[P] %lu %lu %lu ) ", &p, &match, &new_log, &new_certificate);
		if (matches != 4 || match != config->match || board_load(turns, log, new_log, board) != 0) {
			eprintf("# Quitting after illegal play: %.*s\n", received, buffer);
			close(config->channel);
			exit(errno);
		}

		// check for game over
		const enum GameStatus status = board_check(board);
		if (status != GAME_NOT_OVER) {
			board_print(board);
			print_result(status, config);
			game_over = true;
		}

		// move to next turn
		log = new_log;
		certificate = new_certificate;
	}

	if (game_over) return;
	else return play(config, board, !my_turn, turns + 1, log, certificate);
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
		Token password = atoi(argv[4]);
		const int size = snprintf(buffer, sizeof(buffer), "(N %s %lu)\0", username, password);
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
		Token match;
		char opponent_name[MAX_USERNAME_SIZE];
		int matches = sscanf(buffer, " ( %1[W] %lu %31[^)]s ) ", &bw, &match, opponent_name);
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
			printf("# Starting match, press Ctrl-C to quit\n");
			struct Match config = {
				.me = username,         .player2 = username,
				.they = opponent_name,  .player1 = opponent_name,
				.channel = connection,
				.server_address = server_address, .server_port = server_port,
				.match = match, .my_secret = password,
			};
			struct Board board = BOARD_INITIAL;
			play(&config, &board, false, 0, LOG_INITIAL, LOG_INITIAL);
			close(connection);
			goto EXIT_OK;
		}

		// try parsing a "begin playing" response
		char address[128];
		char port[16];
		matches = sscanf(buffer, " ( %1[B] %lu %s %s %[^)]s ) ",
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
			printf("# Starting match, press Ctrl-C to quit\n");
			struct Match config = {
				.me = username,         .player1 = username,
				.they = opponent_name,  .player2 = opponent_name,
				.channel = channel,
				.server_address = server_address, .server_port = server_port,
				.match = match, .my_secret = password,
			};
			struct Board board = BOARD_INITIAL;
			play(&config, &board, true, 0, LOG_INITIAL, LOG_INITIAL);
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
