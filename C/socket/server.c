#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include <sys/socket.h> // socket, connect, bind, listen, accept, recv
#include <netinet/in.h> // sockaddr_in
#include <arpa/inet.h> // inet_ntoa, ntohs
#include <unistd.h> // close

#include "shared.h"


struct Player {
	char username[MAX_USERNAME_SIZE];
	token_t password;
	unsigned score;
	struct Player *next;
};

struct Player *user_add(struct Player **playerbase, const char *username, token_t password)
{
	struct Player *player = calloc(1, sizeof(struct Player));
	strncpy(player->username, username, MAX_USERNAME_SIZE - 1);
	player->password = password;
	player->next = *playerbase;
	player->score = 0;
	*playerbase = player;
	return player;
}

const struct Player *user_find(const struct Player *playerbase, const char *username)
{
	if (playerbase == NULL) return NULL;
	else if (strncmp(playerbase->username, username, MAX_USERNAME_SIZE) == 0) return playerbase;
	else return user_find(playerbase->next, username);
}


int main(const int argc, const char *const argv[])
{
	char buffer[MAX_MESSAGE_SIZE];

	// validate CLI usage
	struct sockaddr_in server;
	if (argc != 3 || parse_address(argv[1], argv[2], &server) != 0) {
		eprintf("Usage: %s <address> <port>\n", argv[0]);
		return 1;
	}

	// initialize server state
	struct Player* playerbase = NULL;
	struct {
		const struct Player* player;
		int connection;
	} waiting = { NULL, -1 };
	srand(time(NULL));

	// start the server
	const int channel = socket(AF_INET, SOCK_STREAM, 0);
	int err = 0;
	err = bind(channel, (struct sockaddr *)&server, sizeof(server));
	err = listen(channel, 30);
	if (err) {
		eprintf("Failed to listen on %s:%s through socket %d\n", argv[1], argv[2], channel);
		return err;
	}

	for (;;) {
		// wait for a client to connect
		eprintf("\nWaiting for client(s), press Ctrl-C to quit\n");
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

		// parse request
		char type;
		char username[MAX_USERNAME_SIZE];
		token_t password;
		const int match = sscanf(buffer, " ( %1[N] %31s %u ) ", &type, username, &password);
		if (match != 3) {
			eprintf("Dropping connection (%d) after invalid message: %.*s\n",
			        connection, received, buffer);
			close(connection);
			continue;
		}

		// evaluate request
		const struct Player *player = user_find(playerbase, username);
		if (player == NULL) {
			eprintf("Registered user %s\n", username);
			player = user_add(&playerbase, username, password);
		} else if (player->password != password) {
			eprintf("Failed login attempt for user %s\n", username);
			close(connection);
			continue;
		}

		// either put a player on hold
		if (waiting.player == NULL) {
			eprintf("Queuing up player %s\n", player->username);
			waiting.player = player;
			waiting.connection = connection;
			continue;
		} else if (strncmp(waiting.player->username, player->username, MAX_USERNAME_SIZE) == 0) {
			eprintf("Stopped user %s from connecting to the lobby twice\n", username);
			close(connection);
			continue;
		}

		// or start a match
		eprintf("Starting match: %s vs %s\n", waiting.player->username, player->username);
		const token_t game = rand();

		// player 2 knows the match token. he should listen for 1
		int size = snprintf(buffer, sizeof(buffer), "(W %u)\0", game);
		ssize_t sent = send(connection, buffer, size, 0);
		if (sent < 0) eprintf("Failed to send reply on connection (%d)\n", connection);

		// player 1 knows the match token and how to reach player 2
		size = snprintf(buffer, sizeof(buffer), "(S %u %s %d)\0",
		                game, inet_ntoa(client.sin_addr), ntohs(client.sin_port));
		sent = send(waiting.connection, buffer, size, 0);
		if (sent < 0) eprintf("Failed to send reply on connection (%d)\n", waiting.connection);

		// close connections
		close(waiting.connection);
		close(connection);
		waiting.player = NULL;
	}

	// exit gracefully
	close(channel);
	return 0;
}
