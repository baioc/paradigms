% Plots np points for each Spline defined by its coefficients a, b, c and d, plus
% the know point xi used during interpolation. Returns the full drawing (Xp, Yp).
function [Xp, Yp] = spval(A, B, C, D, Xi, np=4)

	Xp = [];
	Yp = [];

	for i = 1 : length(Xi) - 1

		Xs = Xi(i) : (Xi(i+1) - Xi(i)) / np : Xi(i+1);

		for j = 1 : length(Xs)
			Ys(j) = A(i)*(Xs(j) - Xi(i))^3 + B(i)*(Xs(j) - Xi(i))^2 + C(i)*(Xs(j) - Xi(i)) + D(i);
		end

		Xp = [Xp Xs];
		Yp = [Yp Ys];
	end

end
