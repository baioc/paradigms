function [X, flops] = solveLUcrout(A, R)

	flops = 0;

	[n, sys] = size(R);
	R = R';
	X = [];

	for j = 1 : sys

		B = R(j,1:end)

		[L, U, B, kLU] = decompLUcrout(A, B);
		[C, kC] = solveC(L, B);
		[Xj, kX] = solveX(U, C);
		flops += kLU + kC + kX;

		X = [X Xj];

	end

end
