#ifndef H_SHARED
#define H_SHARED

#include <stdint.h>

#include <netinet/in.h> // sockaddr_in


/**
 * Fills an address structure from strings containing IP and port numbers.
 *
 * Returns 0 on success or 1 (2) when the 1st (2nd) argument is badly formatted.
 */
int parse_address(const char *ip, const char *port, struct sockaddr_in *addr);

/// fprintf(stderr, format, ...)
int eprintf(const char* format, ...);


#define MAX_MESSAGE_SIZE 8192
#define MAX_USERNAME_SIZE 32
#define MAX_PLAYERBASE 100
#define INITIAL_MODULUS 0xbc747fc5 // as seen in openssl tests
#define EMPTY_BOARD 0u

typedef unsigned token_t;

enum MessageType {
	// player -> server
	GAME_GET = 'G', // (G)
	GAME_NEW = 'N', // (N <username> <password>)
	GAME_END = 'E', // (E <match> <game> <player1> <player2> <signature>)

	// server -> player
	GAME_SCORES = 'S', // <printable score board>
	GAME_BEGIN = 'B', // (B <match> <opponent> <addr> <port>)
	GAME_WAIT = 'W', // (W <match> <opponent>)

	// player <-> player
	GAME_PLAY = 'P', // (P <match> <board> <signature>)
};

typedef uint32_t board_t;

#endif // H_SHARED
