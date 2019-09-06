# Atividade Prática A1 - Grafos (INE5413)

**Gabriel Baiocchi de Sant'Anna** - 18100530 <br/>
Ciências da Computação - Universidade Federal de Santa Catarina

## Representação

Primeiramente, foi criado um tipo de dados para grafos simples e ponderados.
Este foi empregado em todos os demais itens na forma de uma biblioteca reutilizável.
Sua estrutura foi desenvolvida na linguagem C++17 e compilada com a ferramenta [Swig](http://www.swig.org/) (versão 4.0.0) para exportar uma interface acessível em Python 3.

O template `Graph<L,W,d>` gera classes para grafos simples e ponderados, direcionados ou não (respectivamente para `d=true` ou `d=false`), que utilizam rótulos de tipo `L` para identificar **unicamente** cada vértice, associando às arestas pesos de tipo `W`.
Algumas restrições se aplicam: deve haver uma implementação de `hash(L)` e de `std::numeric_limits<W>::infinity()` e `W` deve ser comparável e deve poder ser atribuído aos valores `0` e `1`.

Na resolução dos exercícios são usados strings como rótulos e números em ponto flutuante para pesos.
Para armazenar o grafo na forma de **listas de adjacências**, optou-se por utilizar [`std::unordered_map`](https://en.cppreference.com/w/cpp/container/unordered_map), pois são implementados na biblioteca padrão com *hashs* e portanto oferecem operações cuja complexidade assintótica amortizada é constante.

Cada nodo no grafo é registrado como uma chave cujo valor associado é um outro *hash*; esse último mapeando todos os nodos vizinhos do primeiro aos pesos atribuídos ao arco existente entre eles.
Esse esquema, junto com uma variável que conta o número de arestas presentes, basta para capturar todas as relações necessárias à utilização eficiente da estrutura de dados que representa os grafos.

## Buscas

Na busca em largura, foram utilizados dicionários para armazenar o antecessor na árvore de busca e o número de arestas percorridas para cada vértice encontrado.

A estrutura dos antecessores é aproveitada também para indicar quais vértices não foram visitados: os que não tem nenhuma relação no dicionário.
Ao fim do algoritmo, todo o nodo não visitado é mapeado à `None`, como se representando uma nova raíz de árvore de busca já que nunca foi atingido.

## Ciclo Euleriano

Para o algoritmo de Hierholzer optou-se por utilizar um conjunto de arcos já percorridos que é recursivamente preenchido.

A representação do ciclo propriamente dito é feita com uma lista de nodos, facilitando ainda a etapa de junção dos subciclos eulerianos resultantes das chamadas recursivas.

## Caminhos Mínimos

### Bellman-Ford

As mesmas estruturas das buscas foram utilizadas e ao algoritmo visto em aula foi adicionada uma pequena melhoria: se uma iteração completa do laço externo principal não efetuar mudança nenhuma nos relaxamentos, com certeza nenhuma das iterações seguintes o farão e portanto o algoritmo pode terminar de imediato.

### Floyd–Warshall

Sabendo que os rótulos dos grafos podem adotar qualquer tipo `hashable`, a matriz de adjências utilizada no algoritmo foi representada através de um dicionário bidimensional.

## Extra

Todos os demais algoritmos vistos em sala de aula até então foram também implementados.
