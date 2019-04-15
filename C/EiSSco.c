#include <stdio.h>  	// printf
#include <stdbool.h>
#include <math.h>   	// include -lm flag


int fibonacci(int n)
{
	int current = 0;
	int next = 1;
	int result = 1;

	for (int i = 0; i < n; ++i) {
		result = current + next;
		current = next;
		next = result;
	}

	return result;
}

/**
 * Uses Babylonian Method of finding Square Roots. Also found through Newton's.
 * The sqrt of 'x' is the number 'n' such that x = n*n = n^2; x > 0, n > 0
 * Supose we guess a number 'g' that aproximates n with an error of 'e',
 * then n = g + e => x = (g+e)^2 => e = (x - g^2) / (2g + e)
 * for e << g then e ~= (x - g^2) / 2g => e + g = n ~= (x/g + g) / 2
 */
double my_sqrt(double x)
{
	double guess = 1.0;
	while (true) {
		double new_guess = (x/guess + guess) / 2;
		if (guess == new_guess)
			return guess;
		guess = new_guess;
	}
}


void test_fib(void)
{
	{
		int n = 16;
		for (int i = 0; i <= n; ++i) {
			#if DEBUG
				printf("fib(%d) = %d\n", i, fibonacci(i));
			#endif // DEBUG
		}
	}

	{
		int f = 0, i = 0, n = 32768;
		while (f <= n) {
			f = fibonacci(i++);
			#if DEBUG
				printf("fib(%d) = %d <= %d\n", i, f, n);
			#endif // DEBUG
		}
	}
}

void test_sqrt(void)
{
	double x = 2;
	double ss = sqrt(x);
	double s = my_sqrt(x);

	#if DEBUG
		printf("sqrt(%f) = %.20f\nerr = %e%%\n", x, s, (s-ss)/ss * 100);
	#endif // DEBUG
}

int main(int argc, char const *argv[])
{
	test_fib();
	test_sqrt();
	return 0;
}
