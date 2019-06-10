function P = cosTaylor(n)

	P = zeros(1, 2*n + 1);
	P(1) = 1;
	for k = 1 : n
		P(2*k + 1) = (-1)^k / factorial(2*k);
	end

end
