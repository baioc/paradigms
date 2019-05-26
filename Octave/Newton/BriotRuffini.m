% Calculates n remainders R and n quotients Qn (n defaults to 1) by applying
% Briot-Ruffini's method to a polynomial given by Pn at value x
function [R, Qn] = BriotRuffini(Pn, x, n=1)

	n = min(length(Pn), n);

	for i = 1 : n
		g = length(Pn) - 1;
		Qn(i,1) = Pn(1);
		for j = 2 : g + 1
			Qn(i,j) = Qn(i,j-1) * x + Pn(j);
		end
		R(i,1) = Qn(i,g+1);

		Pn = Qn(i,1:g);
	end

	Qn = Qn(1:end-1);

end
