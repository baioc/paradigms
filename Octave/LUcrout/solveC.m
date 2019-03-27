function C = solveC(L, B, n)

    % @TODO: tratar sistemas indeterminados e impossiveis
    
    C(1) = B(1) / L(1,1);
    
    for i = 2 : n
        s = sum(L(i,1:i-1) * C(i-1));
        C(i) = (B(i) - s) / L(i,i);
    end
    
end
