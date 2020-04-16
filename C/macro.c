/** All credits to Bisqwit:
 * <https://www.youtube.com/channel/UCKTehwyGCKF-b2wo0RKwrcg>.
 */

#include <stdio.h>
#include <stdlib.h> // exit
#include <sysexits.h>

#include "generic/stack.h"


// Turns a non-evaluated EXPR into its string representation.
#define STRINGIFY(expr) #expr

// Assert macro, can be turned into dead code by defining the NDEBUG macro.
#ifdef NDEBUG
#	define my_assert(test) ((void)0)
#else
#	define my_assert(test) do { \
		if (!(test)) { \
			printf("%s:%d: %s: Assertion `%s' failed.\n", \
			       __FILE__, __LINE__, __func__, STRINGIFY(test)); \
			exit(EX_SOFTWARE); \
		} \
	} while (0)
#endif

void test_assert(void)
{
	my_assert(1 + 1 == 2);
	my_assert(1 + 1 == 3); // will crash the program
}


// Begins a GOSUB-enabled block, must be closed by GOSUB_END.
#define GOSUB_BEGIN \
	stack_t _gosub_stack; \
	stack_init(&_gosub_stack, 1, sizeof(int)); \
	int _gosub_continuation = 0; \
_gosub_switch: \
	switch (_gosub_continuation) { \
	default:;

// Ends a GOSUB-enabled block.
#define GOSUB_END \
	} \
	stack_destroy(&_gosub_stack, NULL);

// Jumps to a subroutine label inside a GOSUB enabled block.
#define GOSUB(target) do { \
	const int _gosub_return_address = __LINE__; \
	stack_push(&_gosub_stack, (void*)&_gosub_return_address); \
	goto target; \
	case __LINE__:; \
} while (0)

// Returns from the current GOSUB subroutine to the last caller.
#define GOSUB_RETURN() do { \
	stack_pop(&_gosub_stack, &_gosub_continuation); \
	goto _gosub_switch; \
} while (0)

void test_gosub(void)
{
GOSUB_BEGIN
	printf("Program begins!\n");
	GOSUB(test_subroutine);
	printf("Program continues!\n");
	GOSUB(test_subroutine);
	printf("Program ends!\n");
	return;

test_subroutine:
	printf("GOSUB successful!\n");
	GOSUB_RETURN();
GOSUB_END
}


int main(int argc, const char* argv[])
{
	test_gosub();
	test_assert();
	return 0;
}
