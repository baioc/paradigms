function [a b c d] = Spline(x, y)

	% number of resultant splines := number of intervals
	m = length(x) - 1;

	% find steps
	for i = 1 : m
		h(i) = x(i+1) - x(i);
	end

	for i = 2 : m
		B(i,1) = 6 * ((y(i+1) - y(i))/h(i) - (y(i) - y(i-1))/h(i-1));
	end

	% assuming quadratic behaviour on both ends => S(1)=S(2) && S(m+1)=S(m)
	T(2) = 0;
	R(2) = 3*h(1) + 2*h(2);
	D(2) = h(2);
	for i = 3 : m-1
		T(i) = h(i-1);
		R(i) = 2 * (h(i-1) + h(i));
		D(i) = h(i);
	end
	T(m) = h(m-1);
	R(m) = 2*h(m-1) + 3*h(m);
	D(m) = 0;

	% find Second derivatives
	A = diag(T(3:m),-1) + diag(R(2:m),0) + diag(D(2:m-1),+1);
	S(2:m) = A \ B(2:m);
	% quadratic behaviour
	S(1) = S(2);
	S(m+1) = S(m);

	% calculate Spline coefficients
	for i = 1 : m
		a(i) = (S(i+1) - S(i)) / (6 * h(i));
		b(i) = S(i) / 2;
		c(i) = (y(i+1) - y(i))/h(i) - (S(i+1) + 2*S(i))*h(i) / 6;
		d(i) = y(i);
	end

end