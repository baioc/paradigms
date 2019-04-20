#ifndef BAIOC_LIST_HPP
#define BAIOC_LIST_HPP

#include <algorithm>	// std::swap
#include <initializer_list>
#include <cassert>

namespace baioc {

template <typename T>
	// requires Equality_comparable<T>
class List {
 public:
	virtual T& operator[](int index);
	const virtual T& operator[](int index) const;

	virtual void insert(int index, T element);
	virtual T pop(int index);

	virtual void push_back(T element);
	virtual T pop_back();
	virtual T& back();
	virtual void push_front(T element);
	virtual T pop_front();
	virtual T& front();

	virtual int size() const;
	virtual bool empty() const;

	//! when find() fails, it returns a number equal to size()
	virtual int find(const T& element, int from = 0) const;
	virtual int remove(const T& element);
    virtual bool contains(const T& element) const;
	virtual unsigned count(const T& element) const;
};

template <typename T>
void List<T>::push_back(T element) {
	insert(size() - 1, element);
}

template <typename T>
T List<T>::pop_back() {
	assert(!empty());
	return pop(size() - 1);
}

template <typename T>
T& List<T>::back() {
	assert(!empty());
	return (*this)[size() - 1];
}

template <typename T>
void List<T>::push_front(T element) {
	insert(0, element);
}

template <typename T>
T List<T>::pop_front() {
	assert(!empty());
	return pop(0);
}

template <typename T>
T& List<T>::front() {
	assert(!empty());
	return (*this)[0];
}

template <typename T>
bool List<T>::empty() const {
	return size() <= 0;
}

template <typename T>
int List<T>::find(const T& element, int from) const {
	for (; from < size(); ++from) {
		if ((*this)[from] == element)
			break;
	}
	return from;
}

template <typename T>
int List<T>::remove(const T& element) {
	int index = find(element);
	if (index < size())
		pop(index);
	return index;
}

template <typename T>
bool List<T>::contains(const T& element) const {
	int index = find(element);
	return index < size();
}

template <typename T>
unsigned List<T>::count(const T& element) const {
	unsigned count = 0;
	for (int from = 0;
	     (from = find(element, from) + 1) <= size();
	     ++count) {}
	return count;
}

} // baioc

#endif // BAIOC_LIST_HPP
