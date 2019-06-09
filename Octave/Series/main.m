clear;

a = 0;
b = pi / 2;

x = a : 1e-2 : b;
y = cos(x);


% Aproximation with Taylor series
nMaclaurin = 3
pM = fliplr(cosTaylor(nMaclaurin));
polyout(pM, 'x');
yM = polyval(pM, x);

max_err_Maclaurin = max(abs(yM .- y))
printf("\n");


% Aproximation with Tchebychev series
nTchebychev = 3
pT = cosTchebychev(nTchebychev, a, b);
t = (2*x - (b + a)) / (b - a);
yT = Tchebychev(pT, t);

max_err_Tchebychev = max(abs(yT .- y))
printf("\n");


plot(x, abs(yM .- y), 'b', t, abs(yT .- y), 'r');
a = input("Press enter to exit ");
