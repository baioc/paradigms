
printf("\nLU Crout Decomposition\n");

A = [0   1   3;
     1  -1   3;
     2   2   2;]

B = [4   1;
     1   2;
     2   3;]

[x, flops] = solveLUcrout(A, B)

MAR = max(abs(A*x - B))

printf('\n');
