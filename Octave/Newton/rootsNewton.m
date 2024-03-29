% Applies Newton's method to find x such that f(x) ~= zero, f being a
% function that is (mostly) continuous and derivable in [from : step : to]
function X = rootsNewton(f, from, to, tolerance=1e-5, step=1e-2)

	X = aproximateRoots(f, from, to, step);

	for i = 1 : length(X)
		X(i) = iterateNewton(f, X(i), tolerance);
	end

end
