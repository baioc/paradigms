function [C, flops] = solveC(L, B)

    n = length(L);
    flops = 0;

    if abs(L(n,n)) < 1e-14
        printf("Underdetermined or Inconsistent/Impossible System\n");
        C(1) = NaN;
        exit(-1);
    else
        C(1) = B(1) / L(1,1); flops+=1;
    end

    for i = 2 : n
        s = 0;
        for j = 1 : i-1
            s += L(i,j) * C(i-1); flops+=2;
        end
        C(i) = (B(i) - s) / L(i,i); flops+=2;
    end

end
