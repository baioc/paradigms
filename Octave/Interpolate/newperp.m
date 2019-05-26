function Pn = newperp(X, Y)

	n = length(X) - 1;

	k = 1;
		for i = 1 : n
			A(i,k) = (Y(i+1) - Y(i)) / (X(i+k) - X(i));
		end
	for k = 2 : n
		for i = 1 : n+1-k
			A(i,k) = (A(i+1,k-1) - A(i,k-1)) / (X(i+k) - X(i));
		end
	end

	Pn = [Y(1) A(1,1:end)];

end
