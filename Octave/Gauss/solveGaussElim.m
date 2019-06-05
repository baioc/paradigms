function [X, flops] = solveGaussElim(A, B)

	n = length(A);
	A = [A B];
	flops = 0;

	for k = 1 : n-1
		A = pivotLine(k, A, n);

		for i = k+1 : n
			aux = A(i,k) / A(k,k); flops+=1;
			for j = k+1 : n+1
				A(i,j) -= aux * A(k,j); flops+=2;
			end
			A(i,k) = 0;
		end
	end

	if A(n,n) == 0
		if A(n,n+1) == 0
			X(n,1) = 1; % any value of x is a valid solution
			printf("Underdetermined System, solving for x = 1\n");
		else
			X(n,1) = NaN;
			printf("Inconsistent System\n");
			exit(-1);
		end
	end

	X(n,1) = A(n,n+1) / A(n,n); flops+=1;

	for i = n-1:-1:1
		s = 0;
		for j = i+1 : n
			s += A(i,j) * X(j,1); flops+=2;
		end
		X(i,1) = (A(i,n+1) - s) / A(i,i); flops+=2;
	end

end
