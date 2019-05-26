% Evaluates a polynomial Pn(X) given in the Canonical base {1, x, x^2, ..., x^n}
function Y = Horner(Pn, X)

	Y = [];
	for i = 1 : length(X)
		Y(i) = Pn(end);
		for j = length(Pn)-1 : -1 : 1
			Y(i) = Y(i) * X(i) + Pn(j);
		end
	end

end
