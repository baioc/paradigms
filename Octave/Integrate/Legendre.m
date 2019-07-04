function Gm = Legendre(f, a, b)

	% only works for
	m = 2
		C = [1 1];
		t = [-1/sqrt(3) +1/sqrt(3)];

	x = ((b-a)* t + (b+a)) / 2;
	y = f(x);

	Gm = (b-a)/2 * sum(C(1:m) .* y(1:m));

end
