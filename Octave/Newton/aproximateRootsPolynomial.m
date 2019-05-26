% Finds aproximate roots of a polynomial function with coefficients Pn
function X = aproximateRootsPolynomial(Pn, step=1e-2)

	n = length(Pn) - 1;
	X = [];
	root_count = 0;

	% normalization
	Pn(2:end) /= Pn(1);
	Pn(1) = 1;

	% Range given by Cauchy's Theorem
	r = 1 + max(abs(Pn(2:n+1))); % / abs(Pn(1));
	x = -r : step : r;

	% Real roots (using Bolzano's Intermediate Value Theorem)
	ya = Horner(Pn, x(1));
	for i = 2 : length(x)
		yb = Horner(Pn, x(i));

		if ya * yb <= 0
			root_count++;
			X(root_count,1) = (x(i-1) + x(i)) / 2;
		end

		ya = yb;
	end

	% Complex roots (a controlled guess)
	X(root_count+1 : n, 1) = complex(0, r);

end
