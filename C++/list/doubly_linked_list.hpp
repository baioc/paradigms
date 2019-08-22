#ifndef STRUCTURES_DOUBLY_LINKED_LIST_HPP
#define STRUCTURES_DOUBLY_LINKED_LIST_HPP

#include "list.hpp"

#include <initializer_list>
#include <cassert>


namespace structures {

template <typename T>
class DoublyLinkedList : public structures::List<T> {
 public:
	~DoublyLinkedList();
 	DoublyLinkedList() = default;
	DoublyLinkedList(const std::initializer_list<T>& initial);
	DoublyLinkedList(const DoublyLinkedList& origin);
	DoublyLinkedList(DoublyLinkedList&& source);

	DoublyLinkedList& operator=(const DoublyLinkedList& origin);
	DoublyLinkedList& operator=(DoublyLinkedList&& source);
	DoublyLinkedList& operator+=(const DoublyLinkedList& rhs);

	int insert(T element);
	void sort(); //! "For God’s sake, don’t try sorting a linked list."" — Steve Yegge

	T& operator[](int index);
	const T& operator[](int index) const;

	void insert(int index, T element);
	T pop(int index);

	void push_back(T element);
	T pop_back();
	T& back();
	void push_front(T element);
	T pop_front();
	T& front();

	int size() const;
	using List<T>::empty;

	int find(const T& element) const;
	int remove(const T& element);
	unsigned count(const T& element) const;

 private:
	struct DoublyLinkedNode {
		T data;
		DoublyLinkedNode* next{nullptr};
		DoublyLinkedNode* prev{nullptr};
	};

	int size_{0};
	DoublyLinkedNode* head_{nullptr};
	DoublyLinkedNode* tail_{nullptr};

	DoublyLinkedNode* get(int index) const;
	void put(DoublyLinkedNode* node, DoublyLinkedNode* curr);
	void erase(DoublyLinkedNode* node);

	friend void swap(DoublyLinkedList<T>& a, DoublyLinkedList<T>& b)
	{
		using std::swap;
		swap(a.size_, b.size_);
		swap(a.head_, b.head_);
		swap(a.tail_, b.tail_);
	}
};


template <typename T>
inline int DoublyLinkedList<T>::size() const
{
	return size_;
}

template <typename T>
void DoublyLinkedList<T>::push_front(T element)
{
	auto node = new DoublyLinkedNode;
	node->data = std::move(element);
	node->next = head_;

	if (empty())
		tail_ = node;
	else
		head_->prev = node;

	head_ = node;
	++size_;
}

template <typename T>
T DoublyLinkedList<T>::pop_front()
{
	assert(!empty());
	const auto temp = head_->data;
	const auto next = head_->next;
	delete head_;
	head_ = next;

	if (head_ != nullptr)
		head_->prev = nullptr;

	--size_;
	if (empty())
		tail_ = nullptr;

	return temp;
}

template <typename T>
inline T& DoublyLinkedList<T>::front()
{
	assert(!empty());
	return head_->data;
}

template <typename T>
void DoublyLinkedList<T>::push_back(T element)
{
	auto node = new DoublyLinkedNode;
	node->data = std::move(element);
	node->prev = tail_;

	if (empty())
		head_ = node;
	else
		tail_->next = node;

	tail_ = node;
	++size_;
}

template <typename T>
T DoublyLinkedList<T>::pop_back()
{
	assert(!empty());
	const auto temp = tail_->data;
	const auto prev = tail_->prev;
	delete tail_;
	tail_ = prev;

	if (tail_ != nullptr)
		tail_->next = nullptr;

	--size_;
	if (empty())
		head_ = nullptr;

	return temp;
}

template <typename T>
T& DoublyLinkedList<T>::back()
{
	assert(!empty());
	return tail_->data;
}

template <typename T>
typename DoublyLinkedList<T>::DoublyLinkedNode* DoublyLinkedList<T>::get(int index) const
{
	DoublyLinkedNode* target;
	if (index < size_ / 2) {
		target = head_;
		for (int i = 0; i < index; ++i)
			target = target->next;
	} else {
		target = tail_;
		for (int i = size_ - 1; i > index; --i)
			target = target->prev;
	}

	return target;
}

template <typename T>
const T& DoublyLinkedList<T>::operator[](int index) const
{
	assert(index >= 0);
	assert(index < size_);
	return get(index)->data;
}

template <typename T>
T& DoublyLinkedList<T>::operator[](int index)
{
	assert(index >= 0);
	assert(index < size_);
	return get(index)->data;
}

template <typename T>
void DoublyLinkedList<T>::put(DoublyLinkedNode* node, DoublyLinkedNode* curr)
{
	node->next = curr;
	node->prev = curr->prev;

	curr->prev->next = node;
	curr->prev = node;
}

template <typename T>
void DoublyLinkedList<T>::insert(int index, T element)
{
	assert(index >= 0);
	assert(index <= size_);

	if (index == 0 || empty()) {
		push_front(element);
		return;
	} else if (index == size_) {
		push_back(element);
		return;
	}

	auto curr = get(index);
	auto node = new DoublyLinkedNode;
	node->data = std::move(element);
	put(node, curr);

	++size_;
}

template <typename T>
void DoublyLinkedList<T>::erase(DoublyLinkedNode* node)
{
	node->next->prev = node->prev;
	node->prev->next = node->next;
}

template <typename T>
T DoublyLinkedList<T>::pop(int index)
{
	assert(index >= 0);
	assert(index < size_);

	if (index == 0)
		return pop_front();
	else if (index == size_ - 1)
		return pop_back();

	auto curr = get(index);
	const auto temp = curr->data;
	erase(curr);
	delete curr;

	--size_;
	return temp;
}

template <typename T>
int DoublyLinkedList<T>::find(const T& element) const
{
	int i = 0;
	for (auto iter = head_; i < size_ && iter->data != element; ++i)
		iter = iter->next;
	return i;
}

template <typename T>
int DoublyLinkedList<T>::remove(const T& element)
{
	if (empty()) {
		return size_;
	}

	int i = 0;
	auto curr = head_;
	for (; i < size_ && curr->data != element; ++i)
		curr = curr->next;

	if (curr == head_) {
		pop_front();
		return 0;
	} else if (curr == tail_) {
		pop_back();
		return size_;
	} else if (i < size_) {
		erase(curr);
		delete curr;
		--size_;
	}

	return i;
}

template <typename T>
unsigned DoublyLinkedList<T>::count(const T& element) const
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
DoublyLinkedList<T>::~DoublyLinkedList()
{
	while(!empty())
		pop_front();
}

template <typename T>
DoublyLinkedList<T>::DoublyLinkedList(const std::initializer_list<T>& initial)
{
	for(auto&& value : initial)
		push_back(value);
}

template <typename T>
DoublyLinkedList<T>& DoublyLinkedList<T>::operator+=(const DoublyLinkedList<T>& rhs)
{
	auto iter = rhs.head_;
	for (int i = 0; i < rhs.size_; ++i) {
		push_back(iter->data);
		iter = iter->next;
	}
	return *this;
}

template <typename T>
DoublyLinkedList<T> operator+(DoublyLinkedList<T> lhs, const DoublyLinkedList<T>& rhs)
{
	lhs += rhs;
	return lhs;
}

template <typename T>
DoublyLinkedList<T>::DoublyLinkedList(const DoublyLinkedList<T>& origin):
	DoublyLinkedList()
{
	(*this) += origin;
}

template <typename T>
DoublyLinkedList<T>& DoublyLinkedList<T>::operator=(const DoublyLinkedList<T>& origin)
{
	DoublyLinkedList temp(origin);
	swap(*this, temp);
	return *this;
}

template <typename T>
DoublyLinkedList<T>::DoublyLinkedList(DoublyLinkedList<T>&& source):
	DoublyLinkedList()
{
	swap(*this, source);
}

template <typename T>
DoublyLinkedList<T>& DoublyLinkedList<T>::operator=(DoublyLinkedList<T>&& source)
{
	swap(*this, source);
	return *this;
}

template <typename T>
int DoublyLinkedList<T>::insert(T element)
{
	if (empty() || element < head_->data) {
		push_front(element);
		return 0;
	} else if (element >= tail_->data) {
		push_back(element);
		return size_ - 1;
	}

	auto node = new DoublyLinkedNode;
	node->data = std::move(element);

	int i = size_ - 1;
	auto curr = tail_->prev;
	for (; i >= 0 && curr->data > element; --i)
		curr = curr->prev;

	put(node, curr->next);
	++size_;
	return i;
}

template <typename T>
void DoublyLinkedList<T>::sort()
{
	const auto size = size_;
	DoublyLinkedList<T> aux;

	for (int i = 0; i < size; ++i)
		aux.push_front(pop_back());

	for (int i = 0; i < size; ++i)
		insert(aux.pop_back());
}

} // structures

#endif // STRUCTURES_DOUBLY_LINKED_LIST_HPP
