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

% plot(x, y, 'x'); hold on    	% discrete values
% plot(Xp, Ye, 'g'); hold on  	% exact values
% plot(Xp, Yp_C, 'b'); hold on	% interpolated values

% plot(Xp, abs(Yp_C .- Ye), 'or'); hold on	% error
% plot(Xp, abs(Yp_L .- Ye), 'ob'); hold on	% error
% plot(Xp, abs(Yp_N .- Ye), 'og'); hold on	% error


clear
% Drawing (a duck's back) with Splines
x = [0.9 1.3 1.95 2.15 2.65 3.0 3.90 4.4 4.70 5.00 6.00 7.0 8.0 9.20 10.5 11.3 11.6 12.0 12.7 13.0 13.4];
y = [1.2 1.4 1.85 2.10 2.60 2.7 2.35 2.2 2.05 2.15 2.25 2.3 2.3 1.95 1.40 0.85  0.7  0.6  0.5  0.4  0.3];
[a b c d] = Spline(x, y);

% evaluate each spline in its own interval (split into 4 for ploting)
xsp = []; ysp = [];
for i = 1 : length(x)-1
	xs = x(i) : (x(i+1)-x(i))/4 : x(i+1);
	ys = spval(a(i), b(i), c(i), d(i), x(i), xs);
	xsp = [xsp xs];
	ysp = [ysp ys];
end

% for comparison, a Gregory Newton interpolation with many points
nip = 500;
xip = x(1) : (x(end) - x(1)) / nip : x(end);
yip = GregoryNewton(newperp(x, y), x, xip);

plot(x,y,'*k', xip,yip,'--k', xsp,ysp,'-k','LineWidth', 2);
ylim([-4 8]);
a = input("\nPress enter to close ");


clear;
% Algoritmo Curvas de Bezier
% x = [0 4 4 0]; y = [0 0 4 4]; % parabola horizontal
x = [0 6 -1 5]; y = [0 4 4 0]; % cuspide

cx = 3 * (x(2) - x(1));
bx = 3 * (x(3) - x(2)) - cx;
ax = (x(4) - x(1)) - (cx + bx);

cy = 3 * (y(2) - y(1));
by = 3 * (y(3) - y(2)) - cy;
ay = (y(4) - y(1)) - (cy + by);

np = 100;
t = 0;
for i = 1 : np + 1
	xx(i) = x(1) + t*(cx + t*(bx + t*ax));
	yy(i) = y(1) + t*(cy + t*(by + t*ay));
	t += 1 / np;
end

plot(x,y,'x', x,y,'.-k','linewidth',1, xx,yy,'-k','linewidth',4);
a = input("\nPress enter to exit ");
