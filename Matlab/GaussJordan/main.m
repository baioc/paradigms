
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

[x, flops] = solveGaussElim(A, B)

res = A*x - B;
MAR = max(abs(res))

solution = A\B;
err = x - solution
