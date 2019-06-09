format long;
clear;

f = @(x) log(x)
from = 1;
to = 2;
points = 3;
n = points - 1

x = from : (to - from) / n : to;
y = f(x);

pstep = 1e-2;
Xp = from : pstep : to;
Ye = f(Xp);

% Canonical base Polynomial Interpolation
Pn_C = perp(x, y);
Yp_C = Horner(Pn_C, Xp);
err_max_Vandermonde = max(abs(Yp_C .- Ye))

% Lagrange base Polynomial Interpolation
Pn_L = y;
Yp_L = Lagrange(Pn_L, x, Xp);
err_max_Lagrange = max(abs(Yp_L .- Ye))

% Newton base Polynomial Interpolation
Pn_N = newperp(x, y);
Yp_N = GregoryNewton(Pn_N, x, Xp);
err_max_GregoryNewton = max(abs(Yp_N .- Ye))

figure; plot(x,y,'rx', Xp,Ye,'g', Xp,Yp_C,'--b'); % interpolated polynomial
figure; plot(Xp, abs(Yp_C .- Ye), 'or'); % error


clear;
% Drawing (a duck's back) with Splines
x = [0.9 1.3 1.95 2.15 2.65 3.0 3.90 4.4 4.70 5.00 6.00 7.0 8.0 9.20 10.5 11.3 11.6 12.0 12.7 13.0 13.4];
y = [1.2 1.4 1.85 2.10 2.60 2.7 2.35 2.2 2.05 2.15 2.25 2.3 2.3 1.95 1.40 0.85  0.7  0.6  0.5  0.4  0.3];
[a b c d] = Spline(x, y);

% evaluate each spline in its own interval (split into 4 for ploting)
[xsp, ysp] = spval(a, b, c, d, x);

% for comparison, a Gregory Newton interpolation for the same known points
xip = x(1) : (x(end) - x(1)) / 500 : x(end);
yip = GregoryNewton(newperp(x, y), x, xip);

figure; plot(x,y,'*k', xip,yip,'--k', xsp,ysp,'-k','LineWidth', 2);
ylim([-4 8]);


clear;
% Bezier Curves (cubic)
% x = [0 4 4 0]; y = [0 0 4 4]; % horizontal parabola
% x = [0 6 -1 5]; y = [0 4 4 0]; % cuspid
x = [0 0 9 0]; y = [8 16 8 0];
x2 = [0 4 2 4]; y2 = [0 2 2 0];

[xx, yy] = Bezier3(x, y, 100); % draw path with 100 points
[xx2, yy2] = Bezier3(x2, y2, 100);
xx = [xx xx2]; yy = [yy yy2];
x = [x x2]; y = [y y2];

figure; plot(x,y,'x', x,y,'.-k','linewidth',1, xx,yy,'-k','linewidth',4);

a = input("\nPress enter to exit ");
