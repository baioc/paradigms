function X = solveX(U, C, n)

    X(n) = C(n);
    
    for i = n-1 : -1 : 1
        s = sum(U(i, i+1:n) .* X(i+1:n));
        X(i) = C(i) - s;
    end
    
    X = transpose(X);

end
