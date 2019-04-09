function [X, iterations, flops] = solveGaussSeidelTRID(T, R, D, B, Xo, tolerance, maxIterations, lambda)

    n = length(R);
    flops = 0;

    dif = tolerance + 1; flops += 1;
    iterations = 0;

    kappa = 1 - lambda; flops += 1;

    X = Xo;
    while dif > tolerance && iterations < maxIterations
        flops += 1;

        i = 1;
            X(i) = kappa*X(i) + lambda*((B(i) - D(i)*X(i+1)) / R(i)); flops += 6;

        for i = 2 : n-1
            X(i) = kappa*X(i) + lambda*((B(i) - T(i)*X(i-1) - D(i)*X(i+1)) / R(i)); flops += 8;
        end

        i = n;
            X(i) = kappa*X(i) + lambda*((B(i) - T(i-1)*X(i-1)) / R(i)); flops += 6;

        dif = max(abs(X - Xo)); flops += 3*n;
        Xo = X;

        ++iterations;
    end

end
