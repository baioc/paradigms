function b = cosTchebychev(n, l, u, m=1e2)

	% using m Chebyshev nodes
	j = 1 : m;
	x = cos( ((2*j - 1) * pi) / (2*m) );

	% aproximate cos(x) ~= f(t(x)), where (l <= x <= u) -> (-1 <= t <= +1)
	f = @ (t) cos(((u-l)*t + (u+l)) / 2);

	% calculate starting polynomials and series coefficients
	% using Gauss-Tchebychev integrals
	i = 0;
		s = 0;
		for j = 1 : m
			T(i+1,j) = 1; % T(0) = 1
			s += f(x(j));
		end
		b(i+1) = s / m;
	i = 1;
		s = 0;
		for j = 1 : m
			T(i+1,j) = x(j); % T(1) = x
			s += f(x(j)) * T(i+1,j);
		end
		b(i+1) = 2/m * s;

	% compute the rest of the n+1 with the recursive formula
	for i = 2 : n
		s = 0;
		for j = 1 : m
			T(i+1,j) = 2*x(j)*T(i,j) - T(i-1,j); % T(n+1) = 2*x*T(n) - T(n-1)
			s += f(x(j)) * T(i+1,j);
		end
		b(i+1) = 2/m * s;
	end

end
