/*!
 * @file trie.hpp
 * @author Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
 * @brief Declarações e implementações do template da estrutura de dados Trie.
 * @version 1.0
 * @date 2019-06-23
 *
 * @copyright Copyright (c) 2019
 * @license Apache <https://gitlab.com/baioc/paradigms>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef STRUCTURES_TRIE_HPP
#define STRUCTURES_TRIE_HPP

#include <memory>
#include <vector>
#include <string>
#include <ostream>

//! Estruturas de Dados.
namespace structures {

/**
 * @brief Árvore de Prefixos.
 *
 * Esta estrutura se comporta como um dicionário que utiliza como chaves strings
 * codificadas em ASCII, por padrão com letras de 'a' a 'z', por padrão em caixa
 * baixa (minúsculas) e sempre terminadas com o caractere nulo ('\0').
 * São aceitas como parâmetros tanto strings no estilo C (`const char *`), como
 * também as strings de C++ (`std::string` da STL).
 *
 * @tparam T Tipo de valores armazenados pela Trie, deve ser um tipo regular
 * (possuir construtores de cópia e de move).
 * @tparam R=26 Tamanho do alfabeto de indexação (são 26 letras entre 'a' e 'z').
 * @tparam C='a' Caractere para codificação de índices, alterar 'a' para 'A' faz
 * com que chaves válidas contenham apenas letras em caixa alta (maiúsculas).
 */
template <typename T, unsigned R=26, char C='a'>
class Trie {
 public:
	/**
	 * @brief Armazena um valor na estrutura associando-o à chave dada.
	 * Se já existir um valor associado a esta chave, ele será sobrescrito.
	 *
	 * @param key Chave.
	 * @param val Valor.
	 */
	void put(const char* key, T val);

	/**
	 * @brief Armazena um valor na estrutura associando-o à chave dada.
	 * Se já existir um valor associado a esta chave, ele será sobrescrito.
	 *
	 * @param key Chave.
	 * @param val Valor.
	 */
	void put(const std::string& key, T val);

	/**
	 * @brief Acessa (constante) o elemento associado à chave fornecida.
	 *
	 * @param key Chave.
	 * @return const T* Ponteiro para o valor associado, `nullptr` quando a
	 * chave não existir na Trie.
	 */
	const T* get(const char* key) const;

	/**
	 * @brief Acessa (constante) o elemento associado à chave fornecida.
	 *
	 * @param key Chave.
	 * @return const T* Ponteiro para o valor associado, `nullptr` quando a
	 * chave não existir na Trie.
	 */
	const T* get(const std::string& key) const;

	/**
	 * @brief Acessa o elemento associado à chave fornecida.
	 *
	 * @param key Chave.
	 * @return T* Ponteiro para o valor associado, `nullptr` quando a chave não
	 * existir na Trie.
	 */
	T* get(const char* key);

	/**
	 * @brief Acessa o elemento associado à chave fornecida.
	 *
	 * @param key Chave.
	 * @return T* Ponteiro para o valor associado, `nullptr` quando a chave não
	 * existir na Trie.
	 */
	T* get(const std::string& key);

	/**
	 * @brief Remove da Trie o elemento associado à chave dada.
	 *
	 * @param key Chave.
	 * @return true Se e somente se a chave era válida.
	 * @return false Quando, sem haver o que remover, não houveram alterações.
	 */
	bool remove(const char* key);

	/**
	 * @brief Remove da Trie o elemento associado à chave dada.
	 *
	 * @param key Chave.
	 * @return true Se e somente se a chave era válida.
	 * @return false Quando, sem haver o que remover, não houveram alterações.
	 */
	bool remove(const std::string& key);

	/**
	 * @brief Confere se a árvore contém a chave especificada.
	 *
	 * @param key Chave.
	 * @return true Se e somente se houver algum valor associado à chave.
	 * @return false Quando a chave não existir nesta Trie.
	 */
	bool contains(const char* key) const;

	/**
	 * @brief Confere se a árvore contém a chave especificada.
	 *
	 * @param key Chave.
	 * @return true Se e somente se houver algum valor associado à chave.
	 * @return false Quando a chave não existir nesta Trie.
	 */
	bool contains(const std::string& key) const;

	/**
	 * @brief Confere se a estrutura encontra-se vazia.
	 *
	 * @return true Quando `size() == 0`.
	 * @return false Quando houver ao menos um par chave-valor.
	 */
	bool empty() const;

	/**
	 * @brief Acessa o tamanho da árvore.
	 *
	 * @return int Número de associações chave-valor contidas na Trie.
	 */
	int size() const;

	/**
	 * @brief Adquire a lista de chaves válidas nesse dicionário a partir de um
	 * dado prefixo.
	 * Todas as chaves na estrutura são prefixadas pela string vazia: "".
	 *
	 * @param prefix String utilizada para prefixar as chaves, por padrão uma
	 * string vazia.
	 * @return std::vector<std::string> Vetor de chaves válidas na Trie que
	 * iniciam com o prefixo fornecido.
	 */
	std::vector<std::string> keys(const char* prefix="") const;

	/**
	 * @brief Adquire a lista de chaves válidas nesse dicionário a partir de um
	 * dado prefixo.
	 * Todas as chaves na estrutura são prefixadas pela string vazia: "".
	 *
	 * @param prefix String utilizada para prefixar as chaves, por padrão uma
	 * string vazia.
	 * @return std::vector<std::string> Vetor de chaves válidas na Trie que
	 * iniciam com o prefixo fornecido.
	 */
	std::vector<std::string> keys(const std::string& key) const;

	/**
	 * @brief Limpa a estrutura, tornando-a equivalente a uma recém criada.
	 */
	void clear();

 private:
	//! Nodo da Trie.
	struct TrieNode {
		std::unique_ptr<TrieNode> next_[R]; //!< Arranjo com os próximos nodos.
		std::unique_ptr<T> data_{nullptr}; //!< Valor (dinâmico) contido no nodo.

		//! Adiciona uma entrada à sub-árvore.
		void put(const char* key, T val);
		//! Acessa o valor associado à chave na sub-árvore.
		const T* get(const char* key) const;
		//! Confere se uma chave possui associação nesta sub-árvore.
		bool contains(const char* key) const;
		//! Adquire a lista de chaves válidas nesta sub-árvore.
		std::vector<std::string> keys(const char* prefix, int pos) const;
	};

	//! Remove uma entrada da sub-árvore referenciada.
	static bool remove(std::unique_ptr<TrieNode>& node, const char* key);
	//! Acumula todas as chaves "boas" da sub-árvore em um vetor.
	static void collect(
		const std::unique_ptr<TrieNode>& node,
		const std::string& prefix,
		std::vector<std::string>& keys
	);

	std::unique_ptr<TrieNode> root_{nullptr}; //!< Nodo raíz.
	int size_{0}; //!< Contador de tamanho.
};


template <typename T, unsigned R, char C>
/**
 * @brief Direciona uma Trie à uma stream, exibindo todos os seus pares chave-valor.
 *
 * @param out Stream de saída.
 * @param trie Árvore Digital sendo representada.
 * @return std::ostream& Stream modificada.
 */
std::ostream& operator<<(std::ostream& out, const Trie<T,R,C>& trie) {
	out << "{";
	const auto keys = trie.keys();
	for (size_t i = 0; i < keys.size(); ++i)
		out << (i == 0 ? "\"" : ", \"") << keys[i] << "\": \"" << *trie.get(keys[i]) << '\"';
	out << "}";
	return out;
}


template <typename T, unsigned R, char C>
bool Trie<T,R,C>::empty() const
{
	return size_ <= 0;
}

template <typename T, unsigned R, char C>
bool Trie<T,R,C>::contains(const char* key) const
{
	return root_ && root_->contains(key);
}

template <typename T, unsigned R, char C>
bool Trie<T,R,C>::contains(const std::string& key) const
{
	return contains(key.c_str());
}

template <typename T, unsigned R, char C>
bool Trie<T,R,C>::TrieNode::contains(const char* key) const
{
	if (key[0] == '\0') return !!data_; // !! to force use of operator bool()
	const auto k = key[0] - C;
	return next_[k] && next_[k]->contains(key+1);
}

template <typename T, unsigned R, char C>
const T* Trie<T,R,C>::get(const char* key) const
{
	return root_ ? root_->get(key) : nullptr;
}

template <typename T, unsigned R, char C>
const T* Trie<T,R,C>::get(const std::string& key) const
{
	return get(key.c_str());
}

template <typename T, unsigned R, char C>
const T* Trie<T,R,C>::TrieNode::get(const char* key) const
{
	if (key[0] == '\0') return data_.get();
	const auto k = key[0] - C;
	return next_[k] ? next_[k]->get(key+1) : nullptr;
}

template <typename T, unsigned R, char C>
T* Trie<T,R,C>::get(const char* key)
{
	return const_cast<T*>(const_cast<const Trie&>(*this).get(key));
}

template <typename T, unsigned R, char C>
T* Trie<T,R,C>::get(const std::string& key)
{
	return get(key.c_str());
}

template <typename T, unsigned R, char C>
void Trie<T,R,C>::put(const char* key, T val)
{
	if (!root_) root_ = std::make_unique<TrieNode>();
	root_->put(key, std::move(val));
	++size_;
}

template <typename T, unsigned R, char C>
void Trie<T,R,C>::put(const std::string& key, T val)
{
	put(key.c_str(), std::move(val));
}

template <typename T, unsigned R, char C>
void Trie<T,R,C>::TrieNode::put(const char* key, T val)
{
	if (key[0] == '\0') {
		data_ = std::make_unique<T>(std::move(val));
		return;
	} else {
		const auto k = key[0] - C;
		if (!next_[k]) next_[k] = std::make_unique<TrieNode>();
		next_[k]->put(key+1, std::move(val));
	}
}

template <typename T, unsigned R, char C>
bool Trie<T,R,C>::remove(const char* key)
{
	const bool found = remove(root_, key);
	if (found) --size_;
	return found;
}

template <typename T, unsigned R, char C>
bool Trie<T,R,C>::remove(const std::string& key)
{
	return remove(key.c_str());
}

template <typename T, unsigned R, char C>
bool Trie<T,R,C>::remove(std::unique_ptr<TrieNode>& node, const char* key)
{
	if (!node) {
		return false;
	} else if (key[0] == '\0') {
		node->data_.reset(nullptr);
		for (const auto& child : node->next_)
			if (child) return true;
		node.reset(nullptr);
		return true;
	} else {
		const auto k = key[0] - C;
		const bool found = remove(node->next_[k], key+1);
		if (!node->next_[k] && !node->data_) {
			for (const auto& child : node->next_)
				if (child) return found;
			node.reset(nullptr);
		}
		return found;
	}
}

template <typename T, unsigned R, char C>
int Trie<T,R,C>::size() const
{
	return size_;
}

template <typename T, unsigned R, char C>
std::vector<std::string> Trie<T,R,C>::keys(const char* prefix) const
{
	return root_ ? root_->keys(prefix, 0) : std::vector<std::string>{};
}

template <typename T, unsigned R, char C>
std::vector<std::string> Trie<T,R,C>::keys(const std::string& prefix) const
{
	return keys(prefix.c_str());
}

template <typename T, unsigned R, char C>
std::vector<std::string> Trie<T,R,C>::TrieNode::keys(const char* prefix, int pos) const
{
	if (prefix[pos] == '\0') {
		std::vector<std::string> matches{};
		std::string pre{prefix};
		for (unsigned char c = 0; c < R; ++c)
			collect(next_[c], std::move(pre + static_cast<char>(c + C)), matches);
		return matches;
	} else {
		const auto k = prefix[pos] - C;
		return next_[k] ? next_[k]->keys(prefix, pos+1) : std::vector<std::string>{};
	}
}

template <typename T, unsigned R, char C>
void Trie<T,R,C>::collect(
	const std::unique_ptr<TrieNode>& node,
	const std::string& prefix,
	std::vector<std::string>& keys)
{
	if (!node) return;
	else if (node->data_) keys.push_back(prefix);
	for (unsigned char c = 0; c < R; ++c)
		collect(node->next_[c], std::move(prefix + static_cast<char>(c + C)), keys);
}

template <typename T, unsigned R, char C>
void Trie<T,R,C>::clear()
{
	for (const auto& key : keys())
		remove(key);
}

} // namespace structures

#endif // STRUCTURES_TRIE_HPP
