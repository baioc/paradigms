
printf("\nNewton's Method for Unique Function Roots\n");

f = @(x) x .* tan(x) .- 1
from = 0;
to = 3 * pi;

x = rootsNewton(f, from, to)
err = f(x)


printf("\nNewton's Method for Polynomials\n");

% Pn(x) = a(1)*x^n + a(2)*x^(n-1) + ... + a(n)*x + a(n+1)
coefficients = [0 1 4 6] % <=> 1*x^2 + 4x + 6

[X, M] = rootsNewtonPolynomial(coefficients)
for i = 1 : length(X)
	res(i) = Horner(coefficients, X(i));
end
res

printf("\n");
