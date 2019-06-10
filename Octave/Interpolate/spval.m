function Y = spval(A, B, C, D, Xi, X)

	k = 1;
	for i = 1 : length(D)
		while k <= length(X) && X(i) <= X(k) && X(k) <= Xi(i+1)
			Y(k) = A(i)*(X(k) - Xi(i))^3 + B(i)*(X(k) - Xi(i))^2 + C(i)*(X(k) - Xi(i)) + D(i);
			k++;
		end
	end

end
