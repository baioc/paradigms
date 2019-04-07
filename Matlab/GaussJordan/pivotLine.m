function A = pivotLine(k, A, n)

    Amax = abs(A(k,k));
    iMax = k;
    for i = k+1 : n
        tmp = abs(A(i,k));
        if tmp > Amax
            Amax = tmp;
            iMax = i;
        end
    end

    tmp = A(k,:);
    A(k,:) = A(iMax,:);
    A(iMax,:) = tmp;

end
