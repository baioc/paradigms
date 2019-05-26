format long

f = @(x) log(x)

from = 1
to = 2
points = 40

intervals = points - 1; % = n

x = transpose(from : (to - from) / intervals : to);
y = f(x);

pstep = 1e-2;
Xp = transpose(from : pstep : to);
Ye = f(Xp);

% Canonical base Polynomial Interpolation
Pn_C = perp(x, y);
Yp_C = transpose(Horner(Pn_C, Xp));
max_err_Vandermonde = max(abs(Yp_C .- Ye))

% Lagrange base Polynomial Interpolation
Pn_L = y;
Yp_L = transpose(Lagrange(Pn_L, x, Xp));
max_err_Lagrange = max(abs(Yp_L .- Ye))

% Newton base Polynomial Interpolation
Pn_N = newperp(x, y);
Yp_N = transpose(GregoryNewton(Pn_N, x, Xp));
max_err_GregoryNewton = max(abs(Yp_N .- Ye))

% plot(x, y, 'x',         % discrete values
    %  Xp, Ye, 'g'        % exact values
    %  Xp, Yp_C, 'r');    % interpolated values
% plot(Xp, abs(Yp_N .- Ye), 'or') % error
