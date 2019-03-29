function [X, flops] = solveLUcrout(A, B)

    [L, U, B, kLU] = decompLUcrout(A, B);
    [C, kC] = solveC(L, B);
    [X, kX] = solveX(U, C);

    flops = kLU + kC + kX;

end
