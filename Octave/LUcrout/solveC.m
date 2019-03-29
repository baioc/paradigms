function [C, flops] = solveC(L, B)

    % @TODO: treat impossible and undefined systems

    n = length(L);
    flops = 0;

    C(1) = B(1) / L(1,1); flops+=1;

    for i = 2 : n
        s = 0;
        for j = 1 : i-1
            s += L(i,j) * C(i-1); flops+=2;
        end
        C(i) = (B(i) - s) / L(i,i); flops+=2;
    end

end
