function m = multiplicity(R)

    % multiplicity is at least 1
    m = 1;
    s = abs(R(1) + R(2));

    % if sum is still small, multiplicity is incremented
    while s < 0.1
        m++;
        s += abs(R(m+1));
    end

end
