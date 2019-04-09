function [X, flops] = solveGaussTRID(T, R, D, B)

    n = length(R);
    flops = 0;

    for k = 1 : n-1
        aux = T(k) / R(k); flops+=1;
        R(k+1) -= aux * D(k); flops+=2;
        B(k+1) -= aux * B(k); flops+=2;
    end

    X(n,1) = B(n) / R(n); flops+=1;

    for i = n-1 : -1 : 1
        X(i,1) = (B(i) - D(i) * X(i+1,1)) / R(i); flops+=3;
    end

end
