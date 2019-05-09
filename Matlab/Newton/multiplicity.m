function m = multiplicity(R, tolerance=1e-1)

    % multiplicity is at least 1
    m = 1;
    s = abs(R(1)) + abs(R(2));

    % if sum is still small, multiplicity is incremented
    while s < tolerance
        m++;
        s += abs(R(m+1));
    end

end
