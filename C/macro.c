#include <stdio.h>
#include <stdlib.h> // exit
#include <sysexits.h>

#include "generic/stack.h"


// Turns a non-evaluated EXPR into its string representation.
#define STRINGIFY(EXPR) #EXPR

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
	stack_t __gosub_stack; \
	stack_init(&__gosub_stack, 1, sizeof(int)); \
	int __gosub_continuation = 0; \
__gosub_switch: \
	switch (__gosub_continuation) { \
	default:;

// Ends a GOSUB-enabled block.
#define GOSUB_END \
	} \
	stack_destroy(&__gosub_stack, NULL);

// Jumps to a subroutine label inside a GOSUB enabled block.
#define GOSUB(target) do { \
	const int __gosub_return_address = __LINE__; \
	stack_push(&__gosub_stack, (void*)&__gosub_return_address); \
	goto target; \
	case __LINE__:; \
} while (0)

// Returns from the current GOSUB subroutine to the last caller.
#define GOSUB_RETURN() do { \
	stack_pop(&__gosub_stack, &__gosub_continuation); \
	goto __gosub_switch; \
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


/* Here we use GCC's cleanup attribute and local function definition extensions
 * to setup a definition such that a following curly-brace block will be run
 * when the generated temporary variables goes out of scope. */
#define _DEFER_WITH_NAMES(THUNK_NAME, TEMP_NAME, BODY) \
	auto void THUNK_NAME(void*); \
	int TEMP_NAME __attribute__((cleanup(THUNK_NAME))); \
	void THUNK_NAME(void*_) { BODY }

#define _DEFER_N(n, STMT) \
	_DEFER_WITH_NAMES(__defer_thunk_##n, __defer_temp_##n, STMT)

#define _DEFER(N, STMT) _DEFER_N(N, STMT)

// GCC extension used to generate new symbols for each defer statement.
#define DEFER(STMT) _DEFER(__COUNTER__, STMT)

void test_defer(void)
{
	DEFER(printf("Last!\n");)
	DEFER({
		printf("Second!\n");
		printf("Third!\n");
	})
	printf("First!\n");
}


int main(int argc, const char* argv[])
{
	// test_assert();
	test_gosub();
	test_defer();
	return 0;
}
