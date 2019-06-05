clear;
printf("\nGaussian Elimination\n");

A = [1   1   0   0   0;
     1   4   1   0   0;
     0   1   5   1   0;
     0   0   1   5   1;
     0   0   0   1   1;]

B = [1.5;
     1.0;
     2.0;
     2.0;
     3.0;]

solution = A\B;

[x, flops] = solveGaussElim(A, B)
maximum_absolute_residue = max(abs(A*x - B))


printf("\nGaussian Elimination Optimized for Tridiagonal\n");

n = length(A);
for i = 1 : n-1
	t(i) = A(i+1,i);
	r(i) = A(i,i);
	d(i) = A(i,i+1);
end
r(n) = A(n,n);

t
r
d

[x, flops] = solveGaussTRID(t, r, d, B)
maximum_absolute_residue = max(abs(A*x - B))


printf("\nGauss-Seidel Iterative Method (for Tridiagonal)\n");

initialGuess(1:n,1) = 0;
relaxation = 1;
tolerance = 1e-14;
maxIter = 1597;

[x, iterations, flops] = solveGaussSeidelTRID(t, r, d, B, initialGuess, relaxation, tolerance, maxIter)
maximum_absolute_residue = max(abs(A*x - B))

printf('\n');
