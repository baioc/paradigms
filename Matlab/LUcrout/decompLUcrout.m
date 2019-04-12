function [L, U, B, flops] = decompLUcrout(A, B)

    n = length(A);
    L(n,n) = 0;
    flops = 0;

    k = 1;
    for i = 1 : n
        L(i,1) = A(i,1);
        U(i,i) = 1;
    end

    [A, B, L] = pivotLine(k, A, B, L, n);

    for j = 2 : n
        U(1,j) = A(1,j) / L(1,1); flops+=1;
    end

    % k < n
    for k = 2 : n-1
        for i = k : n
            s = 0;
            for r = 1 : k-1
                s += L(i,r) * U(r,k); flops+=2;
            end
            L(i,k) = A(i,k) - s; flops+=1;
        end

        [A, B, L] = pivotLine(k, A, B, L, n);

        for j = k+1 : n
            s = 0;
            for r = 1 : k-1
                s += L(k,r) * U(r,j); flops+=2;
            end
            U(k,j) = (A(k,j) - s) / L(k,k); flops+=2;
        end
    end

    k = n; i = n;
    s = 0;
    for r = 1 : n-1
        s += L(i,r) * U(r,k); flops+=2;
    end
    L(i,k) = A(i,k) - s; flops+=1;

end
