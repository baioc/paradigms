#ifndef STRUCTURES_BINARY_TREE_HPP
#define STRUCTURES_BINARY_TREE_HPP

#include <memory>
#include <algorithm>
#include <deque>


namespace structures {

template <typename T>
class BinaryTree {
 public:
	void insert(T element);
	bool remove(const T& element);
	bool contains(const T& element) const;
	int size() const;
	bool empty() const;
	int height() const;

	std::deque<T> pre_order() const;
	std::deque<T> in_order() const;
	std::deque<T> post_order() const;

 private:
	struct TreeNode {
		T data_;
		int height_{-1};
		std::unique_ptr<TreeNode> left_{nullptr};
		std::unique_ptr<TreeNode> right_{nullptr};

		explicit TreeNode(T data);

		void insert(T element);
		bool contains(const T& element) const;
		int size() const;
		int height() const;

		std::deque<T> pre_order() const;
		std::deque<T> in_order() const;
		std::deque<T> post_order() const;

		T min() const;
	};

	static bool remove(std::unique_ptr<TreeNode>& node, const T& element);

	std::unique_ptr<TreeNode> root_{nullptr};
};


template <typename T>
BinaryTree<T>::TreeNode::TreeNode(T data):
	data_(std::move(data)), height_(0),
	left_(nullptr), right_(nullptr)
{}

template <typename T>
int BinaryTree<T>::height() const
{
	return root_ ? root_->height() : -1;
}

template <typename T>
int BinaryTree<T>::TreeNode::height() const
{
	return height_;
}

template <typename T>
void BinaryTree<T>::insert(T element)
{
	if (!root_)
		root_ = std::make_unique<TreeNode>(std::move(element));
	else
		root_->insert(std::move(element));
}

template <typename T>
void BinaryTree<T>::TreeNode::insert(T element)
{
	if (element < data_) {
		if (!left_)
			left_ = std::make_unique<TreeNode>(std::move(element));
		else
			left_->insert(std::move(element));
	}
	else if (data_ < element) {
		if (!right_)
			right_ = std::make_unique<TreeNode>(std::move(element));
		else
			right_->insert(std::move(element));
	}

	const auto l = left_ ? left_->height() : -1;
	const auto r = right_ ? right_->height() : -1;
	height_ = std::max(l, r) + 1;
	// @TODO AVL
}

template <typename T>
bool BinaryTree<T>::contains(const T& element) const
{
	return root_ && root_->contains(element);
}

template <typename T>
bool BinaryTree<T>::TreeNode::contains(const T& element) const
{
	if (data_ == element)
		return true;
	else if (element < data_)
		return left_ && left_->contains(element);
	else /*if (data_ < element)*/
		return right_ && right_->contains(element);
}

template <typename T>
int BinaryTree<T>::size() const
{
	return root_ ? root_->size() : 0;
}

template <typename T>
int BinaryTree<T>::TreeNode::size() const
{
	int size = 1;

	if (left_)
		size += left_->size();

	if (right_)
		size += right_->size();

	return size;
}

template <typename T>
bool BinaryTree<T>::empty() const
{
	return !root_;
}

template <typename T>
std::deque<T> BinaryTree<T>::pre_order() const
{
	return root_ ? root_->pre_order() : std::deque<T>();
}

template <typename T>
std::deque<T> BinaryTree<T>::TreeNode::pre_order() const
{
	std::deque<T> nodes(size());

	nodes.push_back(data_);

	if (left_) {
		auto left = left_->pre_order();
		while (!left.empty()) {
			auto temp = left.front();
			nodes.push_back(temp);
			left.pop_front();
		}
	}

	if (right_) {
		auto right = right_->pre_order();
		while (!right.empty()) {
			auto temp = right.front();
			nodes.push_back(temp);
			right.pop_front();
		}
	}

	return nodes;
}

template <typename T>
std::deque<T> BinaryTree<T>::in_order() const
{
	return root_ ? root_->in_order() : std::deque<T>();
}

template <typename T>
std::deque<T> BinaryTree<T>::TreeNode::in_order() const
{
	std::deque<T> nodes(size());

	if (left_) {
		auto left = left_->in_order();
		while (!left.empty()) {
			auto temp = left.front();
			nodes.push_back(temp);
			left.pop_front();
		}
	}

	nodes.push_back(data_);

	if (right_) {
		auto right = right_->in_order();
		while (!right.empty()) {
			auto temp = right.front();
			nodes.push_back(temp);
			right.pop_front();
		}
	}

	return nodes;
}

template <typename T>
std::deque<T> BinaryTree<T>::post_order() const
{
	return root_ ? root_->post_order() : std::deque<T>();
}

template <typename T>
std::deque<T> BinaryTree<T>::TreeNode::post_order() const
{
	std::deque<T> nodes(size());

	if (left_) {
		auto left = left_->post_order();
		while (!left.empty()) {
			auto temp = left.front();
			nodes.push_back(temp);
			left.pop_front();
		}
	}

	if (right_) {
		auto right = right_->post_order();
		while (!right.empty()) {
			auto temp = right.front();
			nodes.push_back(temp);
			right.pop_front();
		}
	}

	nodes.push_back(data_);

	return nodes;
}

template <typename T>
bool BinaryTree<T>::remove(const T& element)
{
	return remove(root_, element);
}

template <typename T>
T BinaryTree<T>::TreeNode::min() const
{
	return left_ ? left_->min() : data_;
}

template <typename T>
bool BinaryTree<T>::remove(std::unique_ptr<TreeNode>& node, const T& element)
{
	if (!node)
		return false;

	bool found = false;

	if (element < node->data_) {
		found = remove(node->left_, element);
	}
	else if (node->data_ < element) {
		found = remove(node->right_, element);
	}
	else if (node->left_ && node->right_) {
		found = true;
		node->data_ = node->right_->min();
		remove(node->right_, node->data_);
	}
	else {
		std::unique_ptr<BinaryTree<T>::TreeNode> tmp{
			node->left_ ? std::move(node->left_)
			            : node->right_ ? std::move(node->right_)
			                           : nullptr
		};
		std::swap(node, tmp);
		return true;
	}

	if (found) {
		const auto l = node->left_ ? node->left_->height() : -1;
		const auto r = node->right_ ? node->right_->height() : -1;
		node->height_ = std::max(l, r) + 1;
		// @TODO AVL
	}

	return found;
}

} // namespace structures

#endif // STRUCTURES_BINARY_TREE_HPP
