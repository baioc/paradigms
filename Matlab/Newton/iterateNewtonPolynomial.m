function [x, m] = iterateNewtonPolynomial(Pn, x, tolerance=1e-14)

    do
        R = BriotRuffini(Pn, x, length(Pn));
        m = multiplicity(R);
        % d(k)Pn(r) = k! * Rr[k+1], but we use k=m instead of k=1
        % (m being the root's multiplicity) to apply L'Hopital's rule, thus:
        % f = factorial(m-1) * R(m-1+1);
        % df = factorial(m) * R(m+1);
        dx = - R(m) / (m * R(m+1)); % - f / df;
        x += dx;
    until abs(dx) < tolerance

end
