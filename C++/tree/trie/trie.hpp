#ifndef STRUCTURES_TRIE_HPP
#define STRUCTURES_TRIE_HPP

#include <memory>
#include <vector>
#include <string>
#include <ostream>

namespace structures {

template <typename T, unsigned R=26, char C='a'>
/**
 * @brief A retrieval data structure using C-style strings as keys which must be
 * null-terminated, ASCII encoded and in lower-case only.
 *
 * Stores values of type T, where T must be a regular type (copy-and-move-constructible).
 */
class Trie {
 public:
	void put(const char* key, T val);
	const T* get(const char* key) const;
	T* get(const char* key);
	bool remove(const char* key);
	bool contains(const char* key) const;
	bool empty() const;
	int size() const;
	std::vector<std::string> keys(const char* prefix = "") const;

 private:
	struct TrieNode {
		std::unique_ptr<TrieNode> next_[R];
		std::unique_ptr<T> data_{nullptr};

		void put(const char* key, T val);
		const T* get(const char* key) const;
		bool contains(const char* key) const;
		std::vector<std::string> keys(const char* prefix, int pos) const;
	};

	static bool remove(std::unique_ptr<TrieNode>& node, const char* key);
	static void collect(
		const std::unique_ptr<TrieNode>& node,
		const std::string& prefix,
		std::vector<std::string>& keys
	);

	std::unique_ptr<TrieNode> root_{nullptr};
	int size_{0};
};


template <typename T, unsigned R, char C>
std::ostream& operator<<(std::ostream& out, const Trie<T,R,C>& trie) {
	out << "{";
	const auto& keys = trie.keys();
	for (size_t i = 0; i < keys.size(); ++i)
		out << (i == 0 ? "\"" : ", \"") << keys[i] << "\": \"" << *trie.get(keys[i].c_str()) << '\"';
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
void Trie<T,R,C>::put(const char* key, T val)
{
	if (!root_) root_ = std::make_unique<TrieNode>();
	root_->put(key, std::move(val));
	++size_;
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

} // namespace structures

#endif // STRUCTURES_TRIE_HPP
