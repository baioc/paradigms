function Y = GregoryNewton(Pn, Xi, X)

	Y = [];

	n = length(Pn);

	for i = 1 : length(X)

		Y(i) = Pn(1);

		for k = 2 : n
			N = 1;
			for j = 1 : k-1
				N *= X(i) - Xi(j);
			end

			Y(i) += Pn(k) * N;
		end

	end

end
