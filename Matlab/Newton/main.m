format long
printf("\n** Trabalho 2: Metodo de Newton para Raizes de Polinomios e outras Funcoes **\n");

printf("\n1) Zeros Reais de Funcoes Gerais\n");

f = @(x) x .* tan(x) - 1;
de = -2 * pi;
ate = 2 * pi;

zeros_reais = rootsNewton(f, de, ate, 1e-14)
% @TODO: residuos f(x)


printf("\n2) Raizes do Polinomio\n");
% Pn(x) = a(1)*x^n + a(2)*x^(n-1) + ... + a(n)*x + a(n+1)
a = [+1 -7 +20.95 -34.75 +34.5004 -20.5012 +6.7512 -0.9504];

printf("  \n2.a) Metodo de Newton tradicional\n")

% @TODO raizes, residuos


printf("  \n2.b) Metodo de Newton com multiplicidade\n")

[x, M] = rootsNewtonPolynomial(a, 1e-9)
% @TODO residuos


printf("  \n2.c) Polinomio fatorado em Binomios\n")

% @TODO fatoracao em binomios


printf("  \n2.d) Resultados do Octave e WolframAlpha\n")

Octave = roots(a)
WolframAlpha = [1.2;
                1.1;
                1.00019;
                complex(0.999907,+0.000160622);
                complex(0.999907,-0.000160622);
                0.9;
                0.8;]


printf("\n3) Metodo de Newton para Sistemas Multivariados\n")

f1 = @(x) x(1)**3 .+ x(2)**3 - 2;
f2 = @(x) sen(x(1)) .* cos(x(2)) - 0.45;
% @TODO valores iniciais
% @TODO solucoes e residuos

printf("\n");
