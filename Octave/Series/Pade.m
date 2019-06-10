function [a b] = Pade(n, m, c) % suposes n = m or n = m+1

	% compute bs with simetric equation system
	for i = 1 : m
		for j = 1 : i
			A(i,j) = c(n-m+i+j);
			A(j,i) = A(i,j);
		end
		B(i,1) = -c(n+i+1);
	end

	b = fliplr(transpose(A \ B));

	% completing b coefficient vector
	b(m+1:n) = 0;
	b = [1 b]; % b0=1

	% calculate as using bs
	a(1) = c(1); % a0 = c0
	for i = 1 : n
		S = c(i+1);
		for j = 1 : i
			S += b(j+1) * c(i-j+1);
		end
		a(i+1) = S;
	end

	% remove trailing zero coefficients
	b = b(1:m+1);

end
