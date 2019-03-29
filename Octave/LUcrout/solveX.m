function [X, flops] = solveX(U, C)

    n = length(C);
    flops = 0;

    X(n,1) = C(n);

    for i = n-1 : -1 : 1
        s = 0;
        for j = i+1 : n
            s += U(i,j) * X(j,1); flops+=2;
        end
        X(i,1) = C(i) - s; flops+=1;
    end

end
