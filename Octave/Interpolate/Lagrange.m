function Y = Lagrange(Pn, Xi, X)

	Y = [];

	n = length(Pn) - 1;

	for k = 1 : length(X)

		Y(k) = 0;
		for i = 1 : n + 1
			L = 1;
			for j = 1 : n + 1
				if (j != i)
					L *= (X(k) - Xi(j)) / (Xi(i) - Xi(j));
				end
			end

			Y(k) += Pn(i) * L;
		end

	end

end
