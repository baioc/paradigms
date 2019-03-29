function [A, B, L] = pivotLine(k, A, B, L, n)

    Lmax = abs(L(k,k));
    iMax = k;
    for i = k+1 : n
        if abs(L(i,k)) > Lmax
            Lmax = L(i,k);
            iMax = i;
        end
    end

    for j = 1 : n
        temp = A(k,j);
        A(k,j) = A(iMax,j);
        A(iMax,j) = temp;

        temp = L(k,j);
        L(k,j) = L(iMax,j);
        L(iMax,j) = temp;
    end

    temp = B(k);
    B(k) = B(iMax);
    B(iMax) = temp;

end
