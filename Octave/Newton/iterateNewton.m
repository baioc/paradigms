function x = iterateNewton(f, x, tolerance=1e-5)

	dx = tolerance;
	do
		fx = f(x);
		df = (f(x+dx) - fx) / dx; % numeric derivative
		dx = - fx / df;
		x += dx;
	until abs(dx) < tolerance

end
