function [X, flops] = solveX(U, C)

	n = length(C);
	flops = 0;

	X(n) = C(n);

	for i = n-1 : -1 : 1
		s = 0;
		for j = i+1 : n
			s += U(i,j) * X(j); flops+=2;
		end
		X(i) = C(i) - s; flops+=1;
	end

	X = transpose(X);

end
