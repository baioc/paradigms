#ifndef BAIOC_LINKED_LIST_HPP
#define BAIOC_LINKED_LIST_HPP

#include "list.hpp"

#include <cassert>


namespace baioc {

class LinkedNode {
 public:
	virtual LinkedNode* next() const { return next_; }
	virtual void link_next(LinkedNode* node) { next_ = node; }
	virtual bool operator==(const LinkedNode& rhs) const;
	virtual bool operator!=(const LinkedNode& rhs) const { return !(*this == rhs); }
	virtual bool operator<(const LinkedNode& rhs) const;
	virtual bool operator>=(const LinkedNode& rhs) const { return !(*this < rhs); }
 private:
	LinkedNode* next_{nullptr};
};

template <>
class List<LinkedNode> {
 public:
	List<LinkedNode>& operator+=(const List<LinkedNode>& rhs) {
		if (&rhs == this) {
			return *this;
		} else if (empty()) {
			*this = rhs;
		} else {
			this->back().link_next(rhs.head_);
			size_ += rhs.size();
			return *this;
		}
	}

	//! sorted insertion
	int insert(LinkedNode& node) {
		if (size() == 0) {
			push_front(node);
			return 0;
		}

		int i = 1;
		auto prev = head_;
		auto curr = head_->next();
		for (; *curr < node && prev != tail_ && i < size(); ++i) {
			prev = curr;
			curr = curr->next();
		}

		if (prev == tail_ || i >= size()) {
			push_back(node);
			return size();
		}

		node.link_next(curr);
		prev->link_next(&node);
		++size_;

		return i;
	}

	void sort(); // @TODO

	void insert(LinkedNode& prev, LinkedNode& node) {
		assert(!empty());

		node.link_next(prev.next());
		prev.link_next(&node);
		++size_;

		if (prev == *tail_)
			tail_ = &node;
	}

	void remove(LinkedNode& prev, LinkedNode& node) {
		assert(!empty());

		prev.link_next(node.next());
		node.link_next(nullptr);
		--size_;

		if (node == *tail_)
			tail_ = &prev;
	}

	LinkedNode& operator[](int index) {
		assert(index >= 0);
		assert(index < size());

		if (index == size() - 1)
			return *tail_;

		auto node = head_;
		for (int i = 0; i < index; ++i)
			node = node->next();
		return *node;
	}

	const LinkedNode& operator[](int index) const {
		assert(index >= 0);
		assert(index < size());

		if (index == size() - 1)
			return *tail_;

		auto node = head_;
		for (int i = 0; i < index; ++i)
			node = node->next();
		return *node;
	}

	void insert(int index, LinkedNode& node) {
		assert(index >= 0);
		assert(index <= size());

		if (index == 0) {
			push_front(node);
			return;
		} else if (index == size()) {
			push_back(node);
			return;
		}

		auto prev = head_;
		for (int i = 1; i < index; ++i)
			prev = prev->next();

		node.link_next(prev->next());
		prev->link_next(&node);
		++size_;
	}

	LinkedNode pop(int index) {
		assert(index >= 0);
		assert(index < size());

		if (index == 0)
			return pop_front();

		auto prev = head_;
		auto curr = head_->next();

		for (int i = 1; i < index; ++i) {
			prev = curr;
			curr = curr->next();
		}

		prev->link_next(curr->next());
		curr->link_next(nullptr);

		if (curr == tail_)
			tail_ = prev;

		--size_;

		return *curr;
	}

	void push_back(LinkedNode& node) {
		if (empty())
			head_ = &node;
		else
			tail_->link_next(&node);
		node.link_next(nullptr); // optional
		tail_ = &node;
		++size_;
	}

	LinkedNode pop_back() { return pop(size() - 1); }

	LinkedNode& back() {
		assert(!empty());
		return *tail_;
	}

	void push_front(LinkedNode& node) {
		if (empty())
			tail_ = &node;
		else
			node.link_next(head_);
		head_ = &node;
		++size_;
	}

	LinkedNode pop_front() {
		assert(!empty());
		auto temp = *head_;
		head_ = head_->next();
		--size_;
		if (empty())
			tail_ = nullptr;
		return temp;
	}

	LinkedNode& front() {
		assert(!empty());
		return *head_;
	}

	int size() const { return size_; }
	bool empty() const { return size() <= 0; }

	int find(const LinkedNode& node) const {
		int i = 0;
		for (auto iter = head_; i < size() && *iter != node; ++i)
			iter = iter->next();
		return i;
	}

	int remove(LinkedNode& node) {
		if (empty()) {
			return -1;
		} else if (node == *head_) {
			pop_front();
			return 0;
		}

		int i = 1;
		auto prev = head_;
		auto curr = head_->next();
		for (; i < size() && *curr != node; ++i) {
			prev = curr;
			curr = curr->next();
		}

		if (i >= size()) {
			return -i;
		} else {
			remove(*prev, *curr);
			return i;
		}
	}

    bool contains(const LinkedNode& node) const {
		const int index = find(node);
		return index < size();
	}

	unsigned count(const LinkedNode& node) const {
		unsigned k = 0;
		auto iter = head_;
		for (int i = 0; i < size(); ++i) {
			if (*iter == node)
				++k;
			iter = iter->next();
		}
		return k;
	}

 private:
	LinkedNode* head_{nullptr};
	LinkedNode* tail_{nullptr};
	int size_{0};
};

template <typename T>
List<LinkedNode> operator+(List<LinkedNode> lhs, const List<LinkedNode>& rhs) {
	lhs += rhs;
	return lhs;
}

} // baioc

#endif // BAIOC_LINKED_LIST_HPP
