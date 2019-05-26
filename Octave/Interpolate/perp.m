% Interpolates a Polynomial function in the Canonical base {1, x, x^2, ..., x^n}
% from given points (x, y) by solving the Vandermonde matrix
function Pn = perp(X, Y)

	k = length(X);

	for i = 1 : k
		V(i,1) = 1;

		for j = 2 : k
			V(i,j) = V(i,j-1) * X(i);
		end
	end

	Pn = V \ Y;

end
