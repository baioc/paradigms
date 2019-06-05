format long;
clear;
printf("\n** Trabalho 2: Metodo de Newton para Raizes de Polinomios e outras Funcoes **\n");

printf("\n1) Zeros Reais de Funcoes Gerais\n");

f = @(x) x .* tan(x) - 1;
de = -2 * pi;
ate = 2 * pi;

zeros_reais = rootsNewton(f, de, ate, 1e-14)
residuos_fx = f(zeros_reais)


printf("\n2) Raizes do Polinomio\n");
% Pn(x) = a(1)*x^n + a(2)*x^(n-1) + ... + a(n)*x + a(n+1)
a = [+1 -7 +20.95 -34.75 +34.5004 -20.5012 +6.7512 -0.9504];

% printf("  \n2.a) Metodo de Newton tradicional\n");

% x_ = rootsNewtonPoly(a, 1e-10)
% for i = 1 : length(x_)
%     residuos_(i,1) = Horner(a, x_(i));
% end
% residuos_


printf("  \n2.b) Metodo de Newton com multiplicidade\n");

[x, M] = rootsNewtonPolynomial(a, 1e-9)
for i = 1 : length(x)
	residuos(i,1) = Horner(a, x(i));
end
residuos


printf("  \n2.c) Polinomio fatorado em Binomios\n");

printf("Pn(x) = ");
for i = 1 : length(x)
	printf("(x - %.1f)^%d", x(i), M(i));
	if i < length(x)
		printf(" * ");
	end
end
printf("\n");


printf("  \n2.d) Resultados do Octave e WolframAlpha\n");

Octave = roots(a)
for i = 1 : length(Octave)
	residuos_Octave(i,1) = Horner(a, Octave(i));
end
residuos_Octave

WolframAlpha = [1.2;
                1.1;
                1.00019;
                complex(0.999907,+0.000160622);
                complex(0.999907,-0.000160622);
                0.9;
                0.8;]
for i = 1 : length(WolframAlpha)
	residuos_WolframAlpha(i,1) = Horner(a, WolframAlpha(i));
end
residuos_WolframAlpha


printf("\n3) Metodo de Newton para Sistemas Multivariados\n");

f1 = @(x) x(1)^3 .+ x(2)^3 - 2;
f2 = @(x) sin(x(1)) .* cos(x(2)) - 0.45;

x_inicial = [1.00; 0.05]

[x_sys, iteracoes] = NewtonSystem2(x_inicial, f1, f2, 1e-12)
residuo_maximo = max(abs([f1(x_sys), f2(x_sys)]))

printf("\n");
