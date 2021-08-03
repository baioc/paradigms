#include "shared.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <arpa/inet.h>  // inet_aton, htons


int parse_address(const char *ip, const char *port, struct sockaddr_in *addr)
{
	const int ok = inet_aton(ip, &addr->sin_addr);
	if (!ok) return 1;

	const int p = atoi(port);
	if (p < 0 || p >= 65536) return 2;

	addr->sin_family = AF_INET;
	addr->sin_port = htons(p);
	return 0;
}

int eprintf(const char* format, ...)
{
	int ret;
	va_list args;
	va_start(args, format);
	ret = vfprintf(stderr, format, args);
	va_end(args);
	return ret;
}

Log log_add(Log log, unsigned turn, unsigned position)
{
	return log | ((uint64_t)(position & 0x0F) << (4 * turn));
}

unsigned log_get(Log log, unsigned turn)
{
	return (log & (0x0Ful << (4 * turn))) >> (4 * turn);
}

Log log_sign(Log log, Token secret)
{
	// Diffie-Hellman-inspired "cryptosystem" (not actually secure)
	log = log % (u_int64_t)SHARED_MODULUS;
	secret = secret;
	Log result = 1;
	for (uint64_t exp = 0; exp < secret; ++exp) {
		result = (result * log) % (u_int64_t)SHARED_MODULUS;
	}
	return log;
}

int board_play(unsigned turn, enum Slot value, unsigned position, struct Board *board, Log *log)
{
	// check if play is valid
	if (position < 1 || position > 9) return -1;
	if (value != BOARD_PLAYER_1 && value != BOARD_PLAYER_2) return -2;
	if (board->slots[position] != BOARD_EMPTY) return -3;

	// then make it
	board->slots[position] = value;
	*log = log_add(*log, turn, position);
	return 0;
}

int board_load(unsigned turn, Log previous, Log next, struct Board *board)
{
	*board = BOARD_INITIAL;
	Log repro = LOG_INITIAL;

	// check if next version of the log matches known previous one
	enum Slot player = BOARD_PLAYER_1;
	for (unsigned i = 0; i < turn; ++i) {
		const unsigned position = log_get(next, i);
		const int err = board_play(i, player, position, board, &repro);
		if (err) return err;
		player = player == BOARD_PLAYER_1 ? BOARD_PLAYER_2 : BOARD_PLAYER_1;
	}
	if (repro != previous) return -1;

	// if it matches up to this point in time, apply the new move
	return board_play(turn, player, log_get(next, turn), board, &repro);
}

enum GameStatus board_check(const struct Board *board)
{
	// count free slots
	int free = 0;
	for (unsigned pos = 1; pos <= 9; ++pos) {
		if (board->slots[pos] == BOARD_EMPTY) free++;
	}

	// check for ways to win
#define CHECK(p, r) \
	if ((board->slots[1] == p && board->slots[2] == p && board->slots[3] == p) \
		|| (board->slots[4] == p && board->slots[5] == p && board->slots[6] == p) \
		|| (board->slots[7] == p && board->slots[8] == p && board->slots[9] == p) \
		|| (board->slots[1] == p && board->slots[4] == p && board->slots[7] == p) \
		|| (board->slots[2] == p && board->slots[5] == p && board->slots[8] == p) \
		|| (board->slots[3] == p && board->slots[6] == p && board->slots[9] == p) \
		|| (board->slots[1] == p && board->slots[5] == p && board->slots[9] == p) \
		|| (board->slots[3] == p && board->slots[5] == p && board->slots[7] == p) \
	) return r

	CHECK(BOARD_PLAYER_1, GAME_PLAYER_1);
	CHECK(BOARD_PLAYER_2, GAME_PLAYER_2);

#undef CHECK

	// otherwise, no one won
	return free == 0 ? GAME_TIE : GAME_NOT_OVER;
}
