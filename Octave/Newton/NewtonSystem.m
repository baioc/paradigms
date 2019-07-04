function X = NewtonSystem(f, X, relax=1, tol=1e-11, maxIter=100, Dx=1e-4)

	n = length(X);
	Dx = Dx * X;
	iter = 0;

	do

		% generating the Jacobian matrix
		Y = f(X);
		Xh = X;

		for j = 1 : n

			% vary a single parameter xj and evaluate f on the updated parameters
			Xh(j) = X(j) + Dx(j);
			Yh = f(Xh);

			% evaluate this derivative for each x in the input vector
			for i = 1 : n
				J(i,j) = (Yh(i) - Y(i)) / Dx(j);
			end

			% return xj to original value for next parameter
			Xh(j) = X(j);

		end

		% Newton's method
		Dx = J \ (-Y);
		X += relax * Dx;

		iter++;

	until max(abs(Dx)) < tol || iter >= maxIter

end
