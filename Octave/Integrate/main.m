format long;

f = @(x) 1 ./ x
a = 1
b = 5
I = log(b) - log(a)
printf("\n");

% Trapezoidal rule
n = 32 % intervals
Tn = trapezoidal(f, a, b, n)
err = abs(Tn - I)
err_ = abs(Tn - trapezoidal(f, a, b, 2*n))
printf("\n");

% Simpson's rule
n = 8 % intervals
Sn = Simpson(f, a, b, n)
err = abs(Sn - I)
err_ = abs(Sn - Simpson(f, a, b, 2*n))
printf("\n");

% Gaussian quadrature
Gm = Legendre(f, a, b)
err = abs(Gm - I)
printf("\n");
