function X = solveLUcrout(A, B)

    n = length(A);
    [L, U, B] = decompLUcrout(A, B, n);
    C = solveC(L, B, n);
    X = solveX(U, C, n);

end
