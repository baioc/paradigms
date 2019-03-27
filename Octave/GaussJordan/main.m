
A = [pi   2.1   3.3;
     pi   2.1  -3.3;
     1.8 -1.6   0.2]

B = [4.1; 
     4.2; 
     2.2]

x = solveGaussJordan(A, B)

y = A*x - B;
mar = max(abs(y))

s = A\B
e = x - s
