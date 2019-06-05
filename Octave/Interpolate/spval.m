% Evaluate a Spline defined by its coefficients a, b, c and d, plus a know point
% xi used during interpolation. This is equivalent to Y = Sp(X).
function Y = spval(a, b, c, d, xi, X)

	for k = 1 : length(X)
		Y(k) = a*(X(k) - xi)^3 + b*(X(k) - xi)^2 + c*(X(k) - xi) + d;
	end

end
