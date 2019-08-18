/*
 * Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
 *
 * @License Apache <https://gitlab.com/baioc/paradigms>
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

 protected:
	struct TreeNode {
		T data_;
		std::unique_ptr<TreeNode> left_{nullptr};
		std::unique_ptr<TreeNode> right_{nullptr};

		explicit TreeNode(T data);

		bool contains(const T& element) const;
		int size() const;

		std::deque<T> pre_order() const;
		std::deque<T> in_order() const;
		std::deque<T> post_order() const;

		T min() const;
	};

	static void insert(std::unique_ptr<TreeNode>& node, T element);
	static bool remove(std::unique_ptr<TreeNode>& node, const T& element);
	static void rotate_left(std::unique_ptr<TreeNode>& node);
	static void rotate_right(std::unique_ptr<TreeNode>& node);
	static int height(const std::unique_ptr<TreeNode>& node);

	std::unique_ptr<TreeNode> root_{nullptr};
};


template <typename T>
BinaryTree<T>::TreeNode::TreeNode(T data):
	data_(std::move(data)),
	left_(nullptr), right_(nullptr)
{}

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
void BinaryTree<T>::insert(T element)
{
	insert(root_, std::move(element));
}

template <typename T>
void BinaryTree<T>::insert(std::unique_ptr<TreeNode>& node, T element)
{
	if (!node) {
		node = std::make_unique<TreeNode>(std::move(element));
		return;
	}

	if (element < node->data_)
		insert(node->left_, std::move(element));
	else if (node->data_ < element)
		insert(node->right_, std::move(element));

	const int balance_factor = height(node->left_) - height(node->right_);
	if (balance_factor > 1) {
		if (node->left_->data_ < element) // left sub-tree is right-heavy
			rotate_left(node->left_);
		rotate_right(node);
	}
	else if (balance_factor < -1) {
		if (element < node->right_->data_) // right sub-tree is left-heavy
			rotate_right(node->right_);
		rotate_left(node);
	}
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
	else /*if (!node->left_ || !node->right_)*/ {
		auto tmp = node->left_ ? std::move(node->left_)
		                       : node->right_ ? std::move(node->right_)
		                                      : nullptr;
		std::swap(node, tmp);
	}

	if (found) {
		const int balance_factor = height(node->left_) - height(node->right_);
		if (balance_factor > 1) {
			if (node->left_->data_ < element) // left sub-tree is right-heavy
				rotate_left(node->left_);
			rotate_right(node);
		}
		else if (balance_factor < -1) {
			if (element < node->right_->data_) // right sub-tree is left-heavy
				rotate_right(node->right_);
			rotate_left(node);
		}
	}

	return found;
}

template <typename T>
int BinaryTree<T>::height() const
{
	return height(root_);
}

template <typename T>
int BinaryTree<T>::height(const std::unique_ptr<TreeNode>& node)
{
	if (node)
		return std::max(height(node->left_), height(node->right_)) + 1;
	return -1;
}

template <typename T>
void BinaryTree<T>::rotate_left(std::unique_ptr<TreeNode>& node)
{
	auto rotated = std::move(node->right_);
	node->right_ = std::move(rotated->left_);
	rotated->left_ = std::move(node);
	std::swap(node, rotated);
}

template <typename T>
void BinaryTree<T>::rotate_right(std::unique_ptr<TreeNode>& node)
{
	auto rotated = std::move(node->left_);
	node->left_ = std::move(rotated->right_);
	rotated->right_ = std::move(node);
	std::swap(node, rotated);
}

} // namespace structures

#endif // STRUCTURES_BINARY_TREE_HPP
