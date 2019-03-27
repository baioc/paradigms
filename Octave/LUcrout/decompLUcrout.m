function [L, U, B] = decompLUcrout(A, B, n)

    L(n,n) = 0;
    
    k = 1;
    for i = 1 : n
        L(i,1) = A(i,1);
        U(i,i) = 1;
    end
    
    [A, B, L] = swapLines(A, B, L, k, n);
    
    for j = 2 : n
        U(1,j) = A(1,j) / L(1,1);
    end
    
    % k < n
    for k = 2 : n-1
        for i = k : n
            s = 0;
            for r = 1 : k-1
                s += L(i,r) * U(r,k);
            end
            L(i,k) = A(i,k) - s;
        end
        
        [A, B, L] = swapLines(A, B, L, k, n);
        
        for j = k+1 : n
            s = 0;
            for r = 1 : k-1
                s += L(k,r) * U(r,j);
            end
            U(k,j) = (A(k,j) - s) / L(k,k);
        end
    end
    
    % k = n, i = n
    s = 0;
    for r = 1 : n-1
        s += L(n,r) * U(r,n);
    end
    L(n,n) = A(n,n) - s;
    
end
