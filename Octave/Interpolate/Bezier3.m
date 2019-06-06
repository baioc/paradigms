function [Xp, Yp] = Bezier3(X, Y, np=100)

	% calculate coefficients
	cx = 3 * (X(2) - X(1));
	bx = 3 * (X(3) - X(2)) - cx;
	ax = (X(4) - X(1)) - (cx + bx);

	cy = 3 * (Y(2) - Y(1));
	by = 3 * (Y(3) - Y(2)) - cy;
	ay = (Y(4) - Y(1)) - (cy + by);

	% parametrize x(t) and y(t)
	t = 0;
	for i = 1 : np + 1
		Xp(i) = X(1) + t*(cx + t*(bx + t*ax));
		Yp(i) = Y(1) + t*(cy + t*(by + t*ay));
		t += 1 / np;
	end

end
