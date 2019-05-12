% Solve equation system by finding the roots of 2 given multivariable functions
function [X, iterations] = NewtonSystem2(X, f1, f2, tolerance=1e-5, maxIter=100)

    iterations = 0;
    dx = [tolerance;
          tolerance;];
    do
        iterations++;
        f1x = f1(X);
        f2x = f2(X);

        B = -[f1x;
              f2x;];

        jacobian(1,1) = (f1([X(1)+dx(1); X(2)      ]) - f1x) / dx(1);
        jacobian(1,2) = (f1([X(1)      ; X(2)+dx(2)]) - f1x) / dx(2);
        jacobian(2,1) = (f2([X(1)+dx(1); X(2)      ]) - f2x) / dx(1);
        jacobian(2,2) = (f2([X(1)      ; X(2)+dx(2)]) - f2x) / dx(2);

        dx = jacobian \ B;
        X += dx;
    until max(abs(dx)) < tolerance || iterations >= maxIter

end
