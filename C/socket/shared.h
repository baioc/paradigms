#ifndef H_SHARED
#define H_SHARED

#include <stdint.h>
#include <stdio.h>

#include <netinet/in.h> // sockaddr_in


/**
 * Fills an address structure from strings containing IP and port numbers.
 *
 * Returns 0 on success or 1 (2) when the 1st (2nd) argument is badly formatted.
 */
int parse_address(const char *ip, const char *port, struct sockaddr_in *addr);

/// fprintf(stderr, format, ...)
int eprintf(const char* format, ...);


// server/shared configs
#define MAX_MESSAGE_SIZE 8192u
#define MAX_USERNAME_SIZE 32u
#define MAX_PLAYERBASE_LEN 100u
#define SHARED_MODULUS 0xbc747fc5U // as seen in openssl tests

typedef uint64_t Token;

enum MessageType {
	// player -> server
	GAME_GET = 'G', // (G)
	GAME_NEW = 'N', // (N <username> <password>)
	GAME_END = 'E', // (E <player1> <player2> <log> <certificate>)

	// server -> player
	GAME_SCORES = 'S', // <printable score board>
	GAME_BEGIN = 'B', // (B <match> <opponent> <addr> <port>)
	GAME_WAIT = 'W', // (W <match> <opponent>)

	// player <-> player
	GAME_PLAY = 'P', // (P <match> <log> <certificate>)
};

enum Slot {
	BOARD_EMPTY    = ' ',
	BOARD_PLAYER_1 = 'X',
	BOARD_PLAYER_2 = 'O',
};

struct Board { enum Slot slots[10]; };

#define BOARD_INITIAL (struct Board){ .slots = { \
	[1] = BOARD_EMPTY, [2] = BOARD_EMPTY, [3] = BOARD_EMPTY, \
	[4] = BOARD_EMPTY, [5] = BOARD_EMPTY, [6] = BOARD_EMPTY, \
	[7] = BOARD_EMPTY, [8] = BOARD_EMPTY, [9] = BOARD_EMPTY, \
} }

enum GameStatus {
	GAME_NOT_OVER = 0,
	GAME_PLAYER_1 = 1,
	GAME_PLAYER_2 = 2,
	GAME_TIE,
};

typedef Token Log;

// one hex digit for each turn, with at most 9 available
#define LOG_INITIAL ((Log)0xFFFFFFF000000000u)

Log log_add(Log log, unsigned turn, unsigned position);
unsigned log_get(Log log, unsigned turn);
Log log_sign(Log log, Token secret);

int board_play(unsigned turn, enum Slot value, unsigned position, struct Board *board, Log *log);
int board_load(unsigned turn, Log previous, Log next, struct Board *board);
enum GameStatus board_check(const struct Board *board);

#endif // H_SHARED
