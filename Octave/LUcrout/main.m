
A = [0  1  3;
     1 -1  3;
     2  2  2]

B = [4;
     1;
     2]

x = solveLUcrout(A, B)

y = A*x - B;
mar = max(abs(y))

s = A\B
e = x - s
