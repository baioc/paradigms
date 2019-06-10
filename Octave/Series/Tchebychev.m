% Evaluates the Tchebychev series given by coefficients b at x
function y = Tchebychev(b, x)

	for k = 1 : length(x)

		% compute the Tchebychev polynomials
		T(1) = 1;    % T0 = 1
		T(2) = x(k); % T1 = x
		for i = 2 : length(b) - 1
			T(i+1) = 2*x(k)*T(i) - T(i-1); % T(n+1) = 2*x*T(n) - T(n-1)
		end

		% evaluate the Tchebychev series at x
		y(k) = sum(b .* T);

	end

end
