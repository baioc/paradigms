
A = [0   1   3;
     1  -1   3;
     2   2   2;]

B = [4;
     1;
     2;]

[x, flops] = solveLUcrout(A, B)

res = A*x - B;
MAR = max(abs(res))

solution = A\B;
err = x - solution
