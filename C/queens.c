#include <stdio.h>

/* We use two's complement integers as sets:
 * 0          : ∅
 * A & B      : A ∩ B
 * A + B      : A ∪ B, when A ∩ B = ∅
 * A - B      : A \ B, when B ⊆ Aqueen
 * ~A         : ~A
 * A & -A     : { min(A) }, when A != ∅
 * ~(~0 << n) : { 0, 1, ..., n−1 }
 * A * 2      : { i + 1 | i ∈ A }
 * A / 2      : { i − 1 | i ∈ A ∧ i != 0 }
 */

int queens(int free_columns, int left_threats, int right_threats)
{
	if (!free_columns) return 1;

	int solutions = 0;
	int safe = free_columns & ~left_threats & ~right_threats;
	int queen;
	while (queen = safe & -safe) {
		solutions += queens(free_columns - queen,
		                    (left_threats + queen) * 2,
		                    (right_threats + queen) / 2);
		safe = safe - queen;
	}

	return solutions;
}

int main()
{
	int n;
	scanf("%d", &n);
	printf("%d\n", queens(~(~0 << n), 0, 0));
}
