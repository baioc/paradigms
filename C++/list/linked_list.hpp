#ifndef STRUCTURES_LINKED_LIST_HPP
#define STRUCTURES_LINKED_LIST_HPP

#include "list.hpp"

#include <initializer_list>
#include <cassert>
#include <iterator>


namespace structures {

template <typename T>
class LinkedList : public structures::List<T> {
 public:
	virtual ~LinkedList();
 	LinkedList() = default;
	LinkedList(const std::initializer_list<T>& initial);
	LinkedList(const LinkedList& origin);
	LinkedList(LinkedList&& source);

	virtual LinkedList& operator=(const LinkedList& origin);
	virtual LinkedList& operator=(LinkedList&& source);
	virtual LinkedList& operator+=(const LinkedList& rhs);

	virtual int insert(T element); //! sorted insertion
	virtual void sort();

	virtual T& operator[](int index);
	virtual const T& operator[](int index) const;

	virtual void insert(int index, T element);
	virtual T pop(int index);

	using List<T>::push_back;
	virtual void push_front(T element);
	virtual T pop_front();
	virtual T& front();

	virtual int size() const;
	using List<T>::empty;

	virtual int find(const T& element) const;
	virtual int remove(const T& element);
	virtual unsigned count(const T& element) const;

 protected:
	struct LinkedNode {
		T data;
		LinkedNode* next{nullptr};
	};

	int size_{0};
	LinkedNode* head_{nullptr};

	LinkedNode* mergesort(LinkedNode* list, int size);
	LinkedNode* merge(LinkedNode* lo, int lo_size, LinkedNode* hi, int hi_size);

	friend void swap(LinkedList<T>& a, LinkedList<T>& b)
	{
		using std::swap;
		swap(a.size_, b.size_);
		swap(a.head_, b.head_);
	}
};

template <typename T>
inline int LinkedList<T>::size() const
{
	return size_;
}

template <typename T>
void LinkedList<T>::push_front(T element)
{
	auto node = new LinkedNode;
	node->data = std::move(element);
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
	return temp;
}

template <typename T>
inline T& LinkedList<T>::front()
{
	assert(!empty());
	return head_->data;
}

template <typename T>
void LinkedList<T>::insert(int index, T element)
{
	assert(index >= 0);
	assert(index <= size_);

	if (index == 0) {
		push_front(element);
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

	const auto temp = curr->data;
	prev->next = curr->next;
	delete curr;

	--size_;
	return temp;
}

template <typename T>
const T& LinkedList<T>::operator[](int index) const
{
	assert(index >= 0);
	assert(index < size_);

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

	auto curr = head_;
	for (int i = 0; i < index; ++i)
		curr = curr->next;
	return curr->data;
}

template <typename T>
int LinkedList<T>::find(const T& element) const
{
	int i = 0;
	for (auto iter = head_; i < size_ && iter->data != element; ++i)
		iter = iter->next;
	return i;
}

template <typename T>
int LinkedList<T>::remove(const T& element)
{
	if (empty()) {
		return size_;
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

	if (i < size_) {
		prev->next = curr->next;
		delete curr;
		--size_;
	}

	return i;
}

template <typename T>
unsigned LinkedList<T>::count(const T& element) const
{
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
LinkedList<T>::~LinkedList()
{
	while (!empty())
		pop_front();
}

template <typename T>
LinkedList<T>::LinkedList(const std::initializer_list<T>& initial)
{
	for (auto iter = std::rbegin(initial); iter != std::rend(initial); ++iter)
		push_front(*iter);
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

	auto node = new LinkedNode;
	node->data = std::move(element);
	node->next = prev->next;
	prev->next = node;

	++size_;
	return i;
}

template <typename T>
void LinkedList<T>::sort()
{
	head_ = mergesort(head_, size_);
	// @NOTE: last node probably does not point to nullptr after sort
}

template <typename T>
typename LinkedList<T>::LinkedNode* LinkedList<T>::mergesort(LinkedNode* list, int size)
{
	if (size <= 1)
		return list;

	const int mid = size / 2;
	const int lim = mid + (size % 2);

	auto high = list;
	for (int i = 0; i < mid; ++i)
		high = high->next;

	list = mergesort(list, mid);
	high = mergesort(high, lim);

	return merge(list, mid, high, lim);
}

template <typename T>
typename LinkedList<T>::LinkedNode* LinkedList<T>::merge(LinkedNode* lo, int lo_size, LinkedNode* hi, int hi_size)
{
	LinkedNode* result = nullptr;

	if (lo_size <= 0)
		return hi;
	else if (hi_size <= 0)
		return lo;

	if (lo->data <= hi->data) {
		result = lo;
		result->next = merge(lo->next, lo_size - 1, hi, hi_size);
	} else {
		result = hi;
		result->next = merge(lo, lo_size, hi->next, hi_size - 1);
	}
	return result;
}

} // structures

#endif // STRUCTURES_LINKED_LIST_HPP
