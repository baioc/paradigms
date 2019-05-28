#ifndef BAIOC_BINARY_TREE_HPP
#define BAIOC_BINARY_TREE_HPP

#include <memory>
#include <algorithm>
#include <deque>


namespace baioc {

template <typename T>
class BinaryTree {
 public:
	void insert(T element);
	bool remove(const T& element);
	bool contains(const T& element) const;
	int size() const;
	bool empty() const;

	std::deque<T> pre_order() const;
	std::deque<T> in_order() const;
	std::deque<T> post_order() const;

 private:
	class TreeNode {
	 public:
		explicit TreeNode(T data): data_(std::move(data)) {}

		void insert(T element);
		bool remove(std::unique_ptr<TreeNode>& root, const T& element);
		bool contains(const T& element) const;
		int size() const;

		std::deque<T> pre_order() const;
		std::deque<T> in_order() const;
		std::deque<T> post_order() const;

		T min() const;

	 private:
		T data_;
		std::unique_ptr<TreeNode> left_{nullptr};
		std::unique_ptr<TreeNode> right_{nullptr};
	};

	std::unique_ptr<TreeNode> root_{nullptr};
};


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
			right_ =std::make_unique<TreeNode>(std::move(element));
		else
			right_->insert(std::move(element));
	}
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
	if (root_)
		return root_->size();
	return 0;
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
	if (root_)
		return root_->pre_order();
	return std::deque<T>();
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
	if (root_)
		return root_->in_order();
	return std::deque<T>();
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
	if (root_)
		return root_->post_order();
	return std::deque<T>();
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
	return root_ && root_->remove(root_, element);
}

template <typename T>
T BinaryTree<T>::TreeNode::min() const
{
	if (left_)
		return left_->min();
	return data_;
}

template <typename T>
bool BinaryTree<T>::TreeNode::remove(
	std::unique_ptr<TreeNode>& root, const T& element)
{
	if (!root)
		return false;

	if (element < root->data_)
		return remove(root->left_, element);
	else if (root->data_ < element)
		return remove(root->right_, element);

	if (root->left_ && root->right_) {
		root->data_ = root->right_->min();
		return remove(root->right_, root->data_);
	}

	std::unique_ptr<BinaryTree<T>::TreeNode> tmp{
		root->left_ ? std::move(root->left_)
		            : root->right_ ? std::move(root->right_)
		                           : nullptr
	};
	std::swap(root, tmp);
	return true;
}

} // namespace baioc

#endif // BAIOC_BINARY_TREE_HPP
