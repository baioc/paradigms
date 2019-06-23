/*!
 * @file main.cpp
 * @author Gabriel B. Sant'Anna
 * @brief Código do programa principal.
 * @version 1.0
 * @date 2019-06-23
 *
 * @copyright Copyright (c) 2019
 */
#include "trie.hpp"

#include <iostream>
#include <string>
#include <fstream>

/*!
 * @brief Programa principal, realiza a leitura e processamento dos arquivos de
 * dicionário com uma Trie, identificando prefixos e indexando o arquivo.
 *
 * Primeiramente, lê um nome de arquivo da entrada padrão e o processa de forma
 * a preencher a estrutura hierárquica da árvore digital.
 * Para tal, é processada uma linha por vez e, supondo caracteres ASCII de 'a' a
 * 'z' em caixa baixa na formatação exemplificada no enunciado do trabalho, cada
 * palavra-chave é associada a um par representando a sua indexação no arquivo
 * original.
 * A seguir, processa cada uma das palavras lidas através da entrada padrão
 * (parando quando receber "0") de forma a disponibilizar como resultado na
 * saída padrão: o resultado da indexação mencionada anteriormente (quando a
 * palavra lida for uma chave) ou uma mensagem indicando se a palavra é um
 * prefixo válido ou não neste dicionário.
 *
 * @return int Algum dos seguintes códigos de erro:
 * 0 quando não houver erros;
 * 1 quando não foi possivel abrir o arquivo lido.
 */
int main()
{
	using namespace std;
	using namespace structures;

	Trie<pair<size_t,size_t>> dict;

	// recebe o nome do arquivo dicionario
	string filename;
	cin >> filename;

	// abre o arquivo
	ifstream file{filename};
	if (!file.is_open()) return 1;

	// para cada linha no arquivo
	size_t cursor = 0;
	for (string line; getline(file, line); cursor += line.length() + 1) {
		// extrai a palavra-chave
		const auto open_pos = line.find('[', 0);
		const auto close_pos = line.find(']', open_pos);
		if (open_pos == string::npos || close_pos == string::npos) continue;
		const auto word = line.substr(open_pos + 1, close_pos - open_pos - 1);
		// coloca a indexacao na trie
		dict.put(word, {cursor, line.length()});
	}

	// recebe prefixos ate que alguma entrada seja "0"
	for (string word; word != "0"; ) {
		cin >> word;
		if (word == "0") {
			break;
		} else if (dict.contains(word)) {
			// quando a palavra estiver presente, recupera a indexacao
			const auto idx = dict.get(word);
			cout << idx->first << ' ' << idx->second << endl;
		} else {
			// se for um prefixo, confere se eh valido
			cout << (dict.keys(word).empty() ? "is not prefix" : "is prefix") << endl;
		}
	}
}
