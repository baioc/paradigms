
printf("\nGaussian Elimination\n");

A = [1   2   0   0   0;
     3   4   5   0   0;
     0   6   7   8   0;
     0   0   9   10  11;
     0   0   0   12  13;]

B = [14;
     15;
     16;
     17;
     18;]

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
maxIter = 1597;
tolerance = 1;
relaxation = 1;

[x, iterations, flops] = solveGaussSeidelTRID(t, r, d, B, initialGuess, tolerance, maxIter, relaxation)
maximum_absolute_residue = max(abs(A*x - B))

printf('\n');
