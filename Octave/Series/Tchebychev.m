% Evaluates the Tchebychev series given by coefficients b at parameter t.
function y = Tchebychev(b, t)

	for k = 1 : length(t)

		% compute the Tchebychev polynomials
		T(1) = 1;    % T0 = 1
		T(2) = t(k); % T1 = t
		for i = 2 : length(b) - 1
			T(i+1) = 2*t(k)*T(i) - T(i-1); % T(n+1) = 2*t*T(n) - T(n-1)
		end

		% evaluate the Tchebychev series at t
		y(k) = sum(b .* T);

	end

end
