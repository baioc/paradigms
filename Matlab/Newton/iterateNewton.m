function x = iterateNewton(f, x, tolerance=1e-5)

    dx = tolerance;
    do
        df = (f(x+dx) - f(x)) / dx; % numeric derivative
        dx = - f(x) / df;
        x += dx;
    until abs(dx) < tolerance

end
