#ifndef BAIOC_LINKED_LIST_HPP
#define BAIOC_LINKED_LIST_HPP

#include "list.hpp"

#include <cassert>
#include <initializer_list>
#include <iostream>


namespace baioc {

template <typename T>
class LinkedList : public baioc::List<T> {
 public:
 	LinkedList() = default;
	LinkedList(const std::initializer_list<T>& initial);
	~LinkedList();

	LinkedList(const LinkedList& origin);
	LinkedList& operator=(const LinkedList& origin);
	LinkedList(LinkedList&& source);
	LinkedList& operator=(LinkedList&& source);
	LinkedList& operator+=(const LinkedList& rhs);

	int insert(T element); //! sorted insertion
	void sort();

	T& operator[](int index);
	const T& operator[](int index) const;

	void insert(int index, T element);
	T pop(int index);

	int size() const;
	using List<T>::empty;

	void push_back(T element);
	T& back();
	void push_front(T element);
	T pop_front();
	T& front();

	int find(const T& element) const;
	int remove(const T& element);
	unsigned count(const T& element) const;

 private:
	struct LinkedNode {
		T data;
		LinkedNode* next{nullptr};
	};

	LinkedNode* head_{nullptr};
	LinkedNode* tail_{nullptr};
	int size_{0};

	void mergesort(LinkedNode* list, int size);
	LinkedNode* merge(LinkedNode* lo, LinkedNode* hi) { return lo; }

	friend void swap(LinkedList<T>& a, LinkedList<T>& b)
	{
		using std::swap;
		swap(a.head_, b.head_);
		swap(a.tail_, b.tail_);
		swap(a.size_, b.size_);
	}
};


template <typename T>
LinkedList<T>::LinkedList(const std::initializer_list<T>& initial)
{
	for (auto&& value : initial)
		push_back(value);
}

template <typename T>
LinkedList<T>::~LinkedList()
{
	while (!empty())
		pop_front();
}

template <typename T>
LinkedList<T>::LinkedList(const LinkedList<T>& origin):
	LinkedList()
{
	(*this) += origin;
}

template <typename T>
LinkedList<T>& LinkedList<T>::operator=(const LinkedList<T>& origin)
{
	LinkedList temp(origin);
	swap(*this, temp);
	return *this;
}

template <typename T>
LinkedList<T>::LinkedList(LinkedList<T>&& source):
	LinkedList()
{
	swap(*this, source);
}

template <typename T>
LinkedList<T>& LinkedList<T>::operator=(LinkedList<T>&& source)
{
	swap(*this, source);
	return *this;
}

template <typename T>
int LinkedList<T>::size() const {
	return size_;
}

template <typename T>
void LinkedList<T>::push_front(T element)
{
	auto node = new LinkedNode;
	node->data = std::move(element);

	if (empty())
		tail_ = node;
	else
		node->next = head_;

	head_ = node;
	++size_;
}

template <typename T>
T LinkedList<T>::pop_front()
{
	assert(!empty());
	const auto temp = head_->data;
	const auto next = head_->next;
	delete head_;
	head_ = next;

	--size_;
	if (empty())
		tail_ = nullptr;

	return temp;
}

template <typename T>
T& LinkedList<T>::front()
{
	assert(!empty());
	return head_->data;
}

template <typename T>
void LinkedList<T>::push_back(T element)
{
	auto node = new LinkedNode;
	node->data = std::move(element);

	if (empty())
		head_ = node;
	else
		tail_->next = node;

	tail_ = node;
	++size_;
}

template <typename T>
T& LinkedList<T>::back()
{
	assert(!empty());
	return tail_->data;
}

template <typename T>
void LinkedList<T>::insert(int index, T element)
{
	assert(index >= 0);
	assert(index <= size_);

	if (index == 0) {
		push_front(element);
		return;
	} else if (index == size_) {
		push_back(element);
		return;
	}

	auto node = new LinkedNode;
	node->data = std::move(element);

	auto prev = head_;
	for (int i = 1; i < index; ++i)
		prev = prev->next;

	node->next = prev->next;
	prev->next = node;
	++size_;
}

template <typename T>
T LinkedList<T>::pop(int index)
{
	assert(index >= 0);
	assert(index < size_);

	if (index == 0)
		return pop_front();

	auto prev = head_;
	auto curr = head_->next;

	for (int i = 1; i < index; ++i) {
		prev = curr;
		curr = curr->next;
	}

	if (curr == tail_)
		tail_ = prev;

	const auto temp = curr->data;
	prev->next = curr->next;
	delete curr;

	--size_;
	return temp;
}

template <typename T>
const T& LinkedList<T>::operator[](int index) const {
	assert(index >= 0);
	assert(index < size_);

	if (index == size_ - 1)
		return tail_->data;

	auto curr = head_;
	for (int i = 0; i < index; ++i)
		curr = curr->next;
	return curr->data;
}

template <typename T>
T& LinkedList<T>::operator[](int index)
{
	assert(index >= 0);
	assert(index < size_);

	if (index == size_ - 1)
		return tail_->data;

	auto curr = head_;
	for (int i = 0; i < index; ++i)
		curr = curr->next;
	return curr->data;
}

template <typename T>
int LinkedList<T>::find(const T& element) const {
	int i = 0;
	for (auto iter = head_; i < size_ && iter->data != element; ++i)
		iter = iter->next;
	return i;
}

template <typename T>
int LinkedList<T>::remove(const T& element)
{
	if (empty()) {
		return -1;
	} else if (element == head_->data) {
		pop_front();
		return 0;
	}

	int i = 1;
	auto prev = head_;
	auto curr = head_->next;
	for (; i < size_ && curr->data != element; ++i) {
		prev = curr;
		curr = curr->next;
	}

	if (i >= size_)
		return -i;
	else if (curr == tail_)
		tail_ = prev;

	prev->next = curr->next;
	delete curr;

	--size_;
	return i;
}

template <typename T>
unsigned LinkedList<T>::count(const T& element) const {
	unsigned k = 0;
	int i = 0;
	for (auto iter = head_; i < size_; ++i) {
		if (iter->data == element)
			++k;
		iter = iter->next;
	}
	return k;
}

template <typename T>
int LinkedList<T>::insert(T element)
{
	if (empty() || element < head_->data) {
		push_front(element);
		return 0;
	}

	int i = 1;
	auto prev = head_;
	for (auto curr = head_->next; i < size_ && curr->data < element; ++i) {
		prev = curr;
		curr = curr->next;
	}

	if (i >= size_) {
		push_back(element);
		return size_;
	}

	auto node = new LinkedNode;
	node->data = std::move(element);
	node->next = prev->next;
	prev->next = node;

	++size_;
	return i;
}

template <typename T>
LinkedList<T>& LinkedList<T>::operator+=(const LinkedList<T>& rhs)
{
	auto iter = rhs.head_;
	for (int i = 0; i < rhs.size_; ++i) {
		push_back(iter->data);
		iter = iter->next;
	}
	return *this;
}

template <typename T>
LinkedList<T> operator+(LinkedList<T> lhs, const LinkedList<T>& rhs)
{
	lhs += rhs;
	return lhs;
}

template <typename T>
void LinkedList<T>::sort() { mergesort(head_, size_); }

template <typename T>
void LinkedList<T>::mergesort(LinkedNode* list, int size)
{
	std::cout << "merging list @" << list->data << " of size " << size << std::endl;
	if (size <= 1)
		return;

	const int mid = size / 2;

	auto high = list;
	for (int i = 0; i < mid; ++i)
		high = high->next;

	mergesort(list, mid);
	mergesort(high, mid + (size % 2));
	list = merge(list, high);
}

// template <typename T>
// LinkedList<T>::LinkedNode* LinkedList<T>::merge(LinkedNode* lo, LinkedNode* hi)
// {
// 	return lo;
// }

} // baioc

#endif // BAIOC_LINKED_LIST_HPP
