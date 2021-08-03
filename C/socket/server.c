#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>

#include <sys/socket.h> // socket, connect, bind, listen, accept, recv, send
#include <netinet/in.h> // sockaddr_in
#include <arpa/inet.h> // inet_ntoa, ntohs
#include <unistd.h> // close
#include <search.h> // POSIX hash tables

#include "shared.h"


static char buffer[MAX_MESSAGE_SIZE];


struct Player {
	char username[MAX_USERNAME_SIZE];
	Token password;
	unsigned score;
	struct Player *next;
};

struct WaitingPlayer {
	const struct Player* player;
	int socket;
};

static struct Player *user_add(struct Player **playerbase, const char *username, Token password)
{
	struct Player *player = calloc(1, sizeof(struct Player));
	strncpy(player->username, username, MAX_USERNAME_SIZE - 1);
	player->password = password;
	player->score = 0;
	player->next = *playerbase;
	*playerbase = player;
	hsearch((ENTRY){ .key = player->username, .data = player }, ENTER);
	return player;
}

static struct Player *user_find(const struct Player *playerbase, const char *username)
{
	ENTRY *e = hsearch((ENTRY){ .key = (void *)username }, FIND);
	return e == NULL ? NULL : e->data;
}

static void users_free(struct Player *playerbase)
{
	if (playerbase == NULL) return;
	struct Player *rest = playerbase->next;
	free(playerbase);
	return users_free(rest);
}


static void handle_get(const int socket, const struct Player *playerbase)
{
	// print scoreboard line by line
	int players = 0;
	int size = 0;
	size += sprintf(buffer + size, "|             Player              | Score |\n");
	size += sprintf(buffer + size, "|---------------------------------|-------|\n");
	while (playerbase != NULL) {
		players++;
		size += sprintf(buffer + size, "| %-31s | %5d |\n", playerbase->username, playerbase->score);
		playerbase = playerbase->next;
	}
	size += sprintf(buffer + size, "\0");

	// send everything and close the connection
	eprintf("Printing out score board with %d players\n", players);
	ssize_t sent = send(socket, buffer, size, 0);
	if (sent < 0) eprintf("Failed to send reply on connection (%d)\n", socket);
	close(socket);
}

static void handle_new(
	const char *username, Token password,
	const int socket, struct sockaddr_in address,
	struct Player **playerbase, struct WaitingPlayer *waiting
) {
	// log in / register
	const struct Player *player = user_find(*playerbase, username);
	if (player == NULL) {
		eprintf("Registered user %s\n", username);
		player = user_add(playerbase, username, password);
	} else if (player->password != password) {
		eprintf("Failed login attempt for user %s\n", username);
		close(socket);
		return;
	}

ENQUEUE_PLAYER:
	// check if player will get put on hold
	if (waiting->player == NULL) {
		eprintf("Queuing up player %s\n", player->username);
		waiting->player = player;
		waiting->socket = socket;
		return;
	} else if (strncmp(waiting->player->username, player->username, MAX_USERNAME_SIZE) == 0) {
		// player was waiting, disconeccted and tried to play again
		waiting->player = NULL;
		goto ENQUEUE_PLAYER;
	}

	// player 1 knows the match token and how to reach player 2
	Token match; do { match = rand(); } while (match % SHARED_MODULUS == 0);
	int size = snprintf(buffer, sizeof(buffer), "(B %lu %s %s %d)\0",
	                    match, player->username, inet_ntoa(address.sin_addr), ntohs(address.sin_port));
	ssize_t sent = send(waiting->socket, buffer, size, 0);
	if (sent < 0) {
		eprintf("Failed to send reply on connection (%d)\n", waiting->socket);
		close(waiting->socket);
		waiting->player = NULL;
		goto ENQUEUE_PLAYER;
	}

	// player 2 knows the match token. he should wait (listen) for 1
	eprintf("Starting match: %s vs %s\n", waiting->player->username, player->username);
	size = snprintf(buffer, sizeof(buffer), "(W %lu %s)\0", match, waiting->player->username);
	sent = send(socket, buffer, size, 0);
	if (sent < 0) eprintf("Failed to send reply on connection (%d)\n", socket);

	// close connections
	close(socket);
	close(waiting->socket);
	waiting->player = NULL;
}

static void handle_end(const char *user1, const char *user2, Log log, Log certificate, struct Player *playerbase)
{
	// ensure both players are different, and in the playerbase
	if (strcmp(user1, user2) == 0) return;
	struct Player *players[] = {
		[1] = user_find(playerbase, user1),
		[2] = user_find(playerbase, user2),
	};
	if (players[1] == NULL || players[2] == NULL) {
		eprintf("Refused match result between invalid players %s and %s\n", user1, user2);
		return;
	}

	// to avoid cheating, verify the game log+certificate using their secrets
	struct Board board = BOARD_INITIAL;
	Log repro = LOG_INITIAL;
	Log cert = LOG_INITIAL;
	int p = 1;
	for (unsigned turn = 0; turn < 9; turn++) {
		const unsigned move = log_get(log, turn);
		if (!move) break;
		const enum Slot s = p == 1 ? BOARD_PLAYER_1 : BOARD_PLAYER_2;
		const int err = board_play(turn, s, move, &board, &repro);
		if (err) break;
		cert = log_add(cert, turn, move);
		cert = log_sign(cert, players[p]->password);
		p = (p % 2) + 1;
	}
	if (repro != log || cert != certificate) {
		eprintf("Refused illegitimate result of %s vs %s\n", user1, user2);
		return;
	}

	// update scores
	switch (board_check(&board)) {
	case GAME_PLAYER_1:
		players[1]->score += 3;
		break;
	case GAME_PLAYER_2:
		players[2]->score += 3;
		break;
	case GAME_TIE:
		players[1]->score += 1;
		players[2]->score += 1;
		break;
	default:
		eprintf("Refused unexpected result of %s vs %s\n", user1, user2);
		return;
	}
	eprintf("Registered result of %s vs %s match\n", user1, user2);
}


int main(const int argc, const char *const argv[])
{
	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 3 || parse_address(argv[1], argv[2], &server) != 0) {
		eprintf("Usage: %s <address> <port>\n", argv[0]);
		return 1;
	}

	// initialize server state
	hcreate(MAX_PLAYERBASE_LEN);
	struct Player *playerbase = NULL;
	struct WaitingPlayer waiting = { .player = NULL };
	srand(time(NULL));

	// start the server
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	int err = 0;
	err = bind(channel, (struct sockaddr *)&server, sizeof(server));
	err = listen(channel, 30);
	if (err) {
		eprintf("Failed to listen on %s:%s through socket %d\n", argv[1], argv[2], channel);
		return errno;
	}
	eprintf("Server is initialized and listening on %s:%s\n", argv[1], argv[2]);
	for (;;) {
		// wait for a client to connect
		eprintf("\nWaiting for client, press Ctrl-C to quit\n");
		struct sockaddr_in client;
		socklen_t client_len = sizeof(client);
		const int connection = accept(channel, (struct sockaddr *)&client, &client_len);
		if (connection < 0) {
			eprintf("Couldn't accept incoming connection\n");
			continue;
		}
		eprintf("Incoming connection (%d) from %s:%d\n",
		        connection, inet_ntoa(client.sin_addr), ntohs(client.sin_port));

		// wait for request
		memset(buffer, 0, sizeof(buffer));
		const ssize_t received = recv(connection, buffer, sizeof(buffer), 0);
		if (received <= 0) {
			eprintf("Client down, closing connection (%d)\n", connection);
			close(connection);
			continue;
		}

		// try parsing a "get scoreboard" request
		char g;
		int matches = sscanf(buffer, " ( %1[G] ) ", &g);
		if (matches == 1) {
			handle_get(connection, playerbase);
			close(connection);
			continue;
		}

		// try parsing a "new game" request
		char n;
		char username[MAX_USERNAME_SIZE];
		Token password;
		matches = sscanf(buffer, " ( %1[N] %31s %lu ) ", &n, username, &password);
		if (matches == 3) {
			handle_new(username, password, connection, client, &playerbase, &waiting);
			continue;
		}

		// try parsing an "end match" request
		char e;
		char player1[MAX_USERNAME_SIZE];
		char player2[MAX_USERNAME_SIZE];
		Log log;
		Log certificate;
		matches = sscanf(buffer, " ( %1[E] %31s %31s %lu %lu ) ", &e, player1, player2, &log, &certificate);
		if (matches == 5) {
			handle_end(player1, player2, log, certificate, playerbase);
			close(connection);
			continue;
		}

		eprintf("Dropping connection (%d) after invalid message: %.*s\n",
		        connection, received, buffer);
		close(connection);
	}

	// exit gracefully
	close(channel);
	users_free(playerbase);
	hdestroy();
	return 0;
}
