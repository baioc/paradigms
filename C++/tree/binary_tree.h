#ifndef BAIOC_BINARY_TREE_HPP
#define BAIOC_BINARY_TREE_HPP

#include <memory>

#include "../list/array_list.hpp"


namespace structures {

template <typename T>
class BinaryTree {
 public:
	void insert(T element);
	bool remove(const T& element);
	bool contains(const T& element) const;
	int size() const;
	bool empty() const;

	baioc::ArrayList<T> pre_order() const;
	baioc::ArrayList<T> in_order() const;
	baioc::ArrayList<T> post_order() const;

 private:
	T data_;
	std::unique_ptr<BinaryTree> left_{nullptr};
	std::unique_ptr<BinaryTree> right_{nullptr};
};


template <typename T>
void BinaryTree<T>::insert(T element)
{
	if (empty()) {
		data_ = element;
	}
	else if (element < data_) {
		if (left_ == nullptr)
			left_ = std::make_unique<BinaryTree>();
		left_->insert(data_);
	}
	else if (data_ < element) {
		if (right_ == nullptr)
			right_ =std::make_unique<BinaryTree>();
		right_->insert(data_);
	}
}

template <typename T>
bool BinaryTree<T>::contains(const T& element) const
{
	if (empty())
		return false;
	else if (data_ == element)
		return true;
	else if (element < data_)
		return left_ != nullptr && left_->contains(element);
	else if (data_ < element)
		return right_ != nullptr && right_->contains(element);
	return false;
}

template <typename T>
int BinaryTree<T>::size() const
{
	int size = left_ != nullptr || right_ != nullptr;

	if (left_ != nullptr)
		size += left_->size();

	if (right_ != nullptr)
		size += right_->size();

	return size;
}

template <typename T>
bool BinaryTree<T>::empty() const
{
	return size() <= 0;
}

template <typename T>
baioc::ArrayList<T> BinaryTree<T>::pre_order() const
{
	baioc::ArrayList<T> nodes(size());

	if (empty())
		return nodes;

	nodes.push_back(data_);

	if (left_ != nullptr)
		nodes += left_->pre_order();

	if (right_ != nullptr)
		nodes += right_->pre_order();

	return nodes;
}

template <typename T>
baioc::ArrayList<T> BinaryTree<T>::in_order() const
{
	baioc::ArrayList<T> nodes(size());

	if (empty())
		return nodes;

	if (left_ != nullptr)
		nodes += left_->pre_order();

	nodes.push_back(data_);

	if (right_ != nullptr)
		nodes += right_->pre_order();

	return nodes;
}

template <typename T>
baioc::ArrayList<T> BinaryTree<T>::post_order() const
{
	baioc::ArrayList<T> nodes(size());

	if (empty())
		return nodes;

	if (left_ != nullptr)
		nodes += left_->pre_order();

	if (right_ != nullptr)
		nodes += right_->pre_order();

	nodes.push_back(data_);

	return nodes;
}

template <typename T>
bool BinaryTree<T>::remove(const T& element)
{
	if (empty()) {
		return false;
	}
	else if (element < data_ && left_ != nullptr) {
		return left_->remove(element);
	}
	else if (data_ < element && right_ != nullptr) {
		return right_->remove(element);
	}
	else if (data_ == element) {
		if (left_ != nullptr && right_ != nullptr) {
			// @TODO node has two children
		}
		else if (left_ != nullptr) {
			// @TODO node has only left child
		}
		else if (right_ != nullptr) {
			// @TODO node has only right child
		}
		return true;
	}
	return false;
}

} // namespace structures

#endif // BAIOC_BINARY_TREE_HPP
