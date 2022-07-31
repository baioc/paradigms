clear;


% Calcula a probabilidade de um processo de Markov estar no estado `target`
% apos exatamente `steps` passos, a partir de um estado inicial `origin` e
% tomando `T` como a matriz de probabilidade de transicao do processo.
function p = ChapmanKolmogorov(T, origin, target, steps)
	assert(steps >= 0);

	% em zero passos, o estado inicial deve ser igual ao alvo para que p != 0
	if steps == 0
		if origin == target
			p = 1.0;
		else
			p = 0.0;
		end

	% em um passo, basta indexar a matrix de transicao
	elseif steps == 1
		p = T(origin, target);

	% em mais de um passo, dividimos o processo em m + n passos, onde
	% os m primeiros passos vao do estado inicial ate algum intermediario e os
	% ultimos n passos vao do estado intermediario ao final. assim, a
	% probabilidade e dada a partir da uniao dos possiveis casos intermediarios
	else
		assert(steps > 1);

		% se o numero de passos for par, dividimos o processo ao meio; se for
		% impar, dividimos em 1 + resto. a divisao nao altera o resultado final
		% (salvo erros de arredondamento de operacoes em ponto flutuante)
		if mod(steps, 2) == 0
			m = steps / 2;
		else
			m = 1;
		end
		n = steps - m;
		assert(m > 0 && n > 0 && m + n == steps);

		% loop acumula a probabilidade do trajeto completo, fazendo a soma de
		% cada caso (disjunto) de possivel estado intermediario; onde a prob.
		% do trajeto e dada pela conjuncao da prob. de cada uma das partes
		s = 0;
		for intermediate = 1 : length(T)
			originToIntermediate = ChapmanKolmogorov(T, origin, intermediate, m);
			intermediateToTarget = ChapmanKolmogorov(T, intermediate, target, n);
			s += originToIntermediate * intermediateToTarget;
		end
		p = s;
	end

	assert(p >= 0.0 && p <= 1.0);
end


% questao 5 do E2: prob. de estar em S9 depois de 4 passos partindo de S3
T = [0.0200 0.5500 0.0300 0.0800 0.0400 0.0400 0.0800 0.0300 0.1100 0.0200;
     0.2700 0.1200 0.0400 0.1200 0.0500 0.0200 0.0400 0.1600 0.1200 0.0600;
     0.1000 0.1500 0.0900 0.2700 0.1300 0.0600 0.0000 0.0100 0.0400 0.1500;
     0.1500 0.1300 0.0800 0.1500 0.0700 0.0500 0.1300 0.1100 0.0800 0.0500;
     0.1400 0.1600 0.1300 0.2500 0.0500 0.0800 0.0500 0.0200 0.1000 0.0200;
     0.1200 0.3000 0.0800 0.0800 0.0700 0.1300 0.0100 0.1500 0.0100 0.0500;
     0.0700 0.0200 0.0400 0.0400 0.0100 0.0800 0.1600 0.3700 0.0600 0.1500;
     0.0400 0.0400 0.1500 0.1300 0.1400 0.0800 0.1200 0.1300 0.0500 0.1200;
     0.1300 0.0500 0.1100 0.1300 0.0900 0.1700 0.0000 0.1600 0.0600 0.1000;
     0.1200 0.1300 0.0400 0.0500 0.1100 0.1300 0.0400 0.1200 0.1200 0.1400];
S3 = 3;
S9 = 9;
passos = 4;
resposta = ChapmanKolmogorov(T, S3, S9, passos)
