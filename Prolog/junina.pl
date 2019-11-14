%% Trabalho II - 27 - https://rachacuca.com.br/logica/problemas/festa-junina/
% Cinco mães ...
mae('Eduarda').
mae('Fatima').
mae('Luciana').
mae('Maria').
mae('Valentina').

% ... estão numa festa junina com os seus filhos, ...
filho('Francisco').
filho('Jose').
filho('Luiz').
filho('Roberto').
filho('Tales').

% ... cujas idades variam.
idade(7).
idade(8).
idade(9).
idade(10).
idade(11).

% Cada uma usa um vestido de vestido diferente ...
vestido(amarelo).
vestido(azul).
vestido(branco).
vestido(verde).
vestido(vermelho).

% ... e trouxe uma comida especifica ...
comida('arroz doce').
comida(cuzcuz).
comida(pamonha).
comida('pe de moleque').
comida(suspiro).

% ... do seu estado de origem, ...
estado('Alagoas').
estado('Bahia').
estado('Ceara').
estado('Paraiba').
estado('Sergipe').

% ... cujas capitais são:
capital('Alagoas', 'Maceio').
capital('Bahia', 'Salvador').
capital('Ceara', 'Fortaleza').
capital('Paraiba', 'Joao Pessoa').
capital('Sergipe', 'Aracaju').

% Regras
aoLado(X,Y,List) :- nextto(X,Y,List); nextto(Y,X,List).
aEsquerda(X,Y,List) :- nth0(IdxX,List,X), nth0(IdxY,List,Y), IdxX < IdxY.
aDireita(X,Y,List) :- aEsquerda(Y,X,List).
exataEsquerda(X,Y,List) :- nth0(IdxX,List,X), IdxY is IdxX + 1, nth0(IdxY,List,Y).
exataDireita(X,Y,List) :- exataEsquerda(Y,X,List).
all_distinct([]).
all_distinct([H|T]) :- not(member(H,T)), all_distinct(T).

% Solucao
solucao(Festa) :-
    Festa = [
        dupla(Vestido1, Mae1, Filho1, Idade1, Comida1, Estado1),
        dupla(Vestido2, Mae2, Filho2, Idade2, Comida2, Estado2),
        dupla(Vestido3, Mae3, Filho3, Idade3, Comida3, Estado3),
        dupla(Vestido4, Mae4, Filho4, Idade4, Comida4, Estado4),
        dupla(Vestido5, Mae5, Filho5, Idade5, Comida5, Estado5)
    ],

    % A mae que levou suspiro para a festa nasceu no estado cuja capital eh Maceio.
    capital(EstadoMaceio,'Maceio'),
    member(dupla(_,_,_,_,suspiro,EstadoMaceio), Festa),

    % A mae do garoto mais velho esta em algum lugar a direita da mulher do vestido azul.
    findall(Idade, idade(Idade), Idades),
    max_list(Idades, MaiorIdade),
    aDireita(dupla(_,_,_,MaiorIdade,_,_), dupla(azul,_,_,_,_,_), Festa),

    % Luiz esta na terceira posicao.
    Filho3 = 'Luiz',

    % Quem levou Cuzcuz esta na segunda posicao.
    Comida2 = cuzcuz,

    % A Valentina esta na terceira posicao.
    Mae3 = 'Valentina',

    % Luciana esta ao lado da mae de Tales.
    aoLado(dupla(_,'Luciana',_,_,_,_), dupla(_,_,'Tales',_,_,_), Festa),

    % A mulher do vestido verde esta exatamente a esquerda da mulher que nasceu na Bahia.
    exataEsquerda(dupla(verde,_,_,_,_,_), dupla(_,_,_,_,_,'Bahia'), Festa),

    % O filho da Luciana tem 10 anos.
    member(dupla(_,'Luciana',_,10,_,_), Festa),

    % Roberto esta exatamente a direita de quem levou suspiro para a festa.
    exataDireita(dupla(_,_,'Roberto',_,_,_), dupla(_,_,_,_,suspiro,_), Festa),

    % Valentina esta exatamente a esquerda de Eduarda.
    exataEsquerda(dupla(_,'Valentina',_,_,_,_), dupla(_,'Eduarda',_,_,_,_), Festa),

    % A mulher do vestido azul esta ao lado da mae do filho de 9 anos.
    aoLado(dupla(azul,_,_,_,_,_), dupla(_,_,_,9,_,_), Festa),

    % Quem levou arroz doce esta exatamente a esquerda da mae que nasceu em Alagoas.
    exataEsquerda(dupla(_,_,_,_,'arroz doce',_), dupla(_,_,_,_,_,'Alagoas'), Festa),

    % A mae do filho de 8 anos esta em algum lugar a direita da mulher do vestido verde.
    aDireita(dupla(_,_,_,8,_,_), dupla(verde,_,_,_,_,_), Festa),

    % A mae de Francisco esta exatamente a esquerda da mae de Luiz.
    exataEsquerda(dupla(_,_,'Francisco',_,_,_), dupla(_,_,'Luiz',_,_,_), Festa),

    % A mulher do vestido amarelo esta exatamente a esquerda da mae do filho de 11 anos.
    exataEsquerda(dupla(amarelo,_,_,_,_,_), dupla(_,_,_,11,_,_), Festa),

    % Fatima esta ao lado da mae que nasceu em Alagoas.
    aoLado(dupla(_,'Fatima',_,_,_,_), dupla(_,_,_,_,_,'Alagoas'), Festa),

    % A mulher que nasceu no Ceara esta exatamente a esquerda da mulher que nasceu no estado cuja capital eh Aracaju.
    capital(EstadoAracaju,'Aracaju'),
    exataEsquerda(dupla(_,_,_,_,_,'Ceara'), dupla(_,_,_,_,_,EstadoAracaju), Festa),

    % A mae de Roberto levou pamonha para a festa.
    member(dupla(_,_,'Roberto',_,pamonha,_), Festa),

    % A mulher do vestido vermelho esta ao lado da mulher que nasceu em Sergipe.
    aoLado(dupla(vermelho,_,_,_,_,_), dupla(_,_,_,_,_,'Sergipe'), Festa),

    % A mae do Jose esta em algum lugar a direita da mulher do vestido verde.
    aDireita(dupla(_,_,'Jose',_,_,_), dupla(verde,_,_,_,_,_), Festa),

    % Testa todas as possibilidades...
    vestido(Vestido1), vestido(Vestido2), vestido(Vestido3), vestido(Vestido4), vestido(Vestido5),
    all_distinct([Vestido1, Vestido2, Vestido3, Vestido4, Vestido5]),

    mae(Mae1), mae(Mae2), mae(Mae3), mae(Mae4), mae(Mae5),
    all_distinct([Mae1, Mae2, Mae3, Mae4, Mae5]),

    filho(Filho1), filho(Filho2), filho(Filho3), filho(Filho4), filho(Filho5),
    all_distinct([Filho1, Filho2, Filho3, Filho4, Filho5]),

    idade(Idade1), idade(Idade2), idade(Idade3), idade(Idade4), idade(Idade5),
    all_distinct([Idade1, Idade2, Idade3, Idade4, Idade5]),

    comida(Comida1), comida(Comida2), comida(Comida3), comida(Comida4), comida(Comida5),
    all_distinct([Comida1, Comida2, Comida3, Comida4, Comida5]),

    estado(Estado1), estado(Estado2), estado(Estado3), estado(Estado4), estado(Estado5),
    all_distinct([Estado1, Estado2, Estado3, Estado4, Estado5]).
