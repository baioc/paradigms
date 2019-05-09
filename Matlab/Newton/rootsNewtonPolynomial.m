function [X, M] = rootsNewtonPolynomial(Pn, tolerance=1e-5, step=1e-2)

	% remove leading zero coefficients
    nez = find(Pn, 1); % list with index of first non-zero coefficient
    if length(nez) == 1
        Pn = Pn(nez:end);
    else
        Pn = 0;
    end

    X = aproximateRootsPolynomial(Pn, step);

	r = 0;
	do
		r++;
		[X(r), M(r,1)] = iterateNewtonPolynomial(Pn, X(r), tolerance);
		for i = 1 : M(r)
			[y Pn] = BriotRuffini(Pn, X(r));
		end
	until length(Pn) - 1 < 1
	X = X(1:r);

end
