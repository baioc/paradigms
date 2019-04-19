#ifndef STRUCTURES_LIST_H
#define STRUCTURES_LIST_H

#include <algorithm>	// std::swap
#include <initializer_list>


namespace structures {

template <typename T>
	// requires Equality_comparable<T>
class List {
 public:
	virtual T& operator[](int) = 0;
	const virtual T& operator[](int) const = 0;

	virtual void insert(int, T) = 0;
	virtual T pop(int) = 0;

	virtual int size() const = 0;
	virtual bool empty() const;

	//! when find() fails, it returns a number equal to size()
	virtual int find(const T&, int = 0) const;
	virtual int remove(const T&);
    virtual bool contains(const T&) const;
	virtual unsigned count(const T&) const;
};


template <typename T>
bool List<T>::empty() const
{
	return size() <= 0;
}

template <typename T>
//! when find() fails, it returns a number equal to size()
int List<T>::find(const T& element, int from) const
{
	for (; from < size(); ++from) {
		if ((*this)[from] == element)
			break;
	}
	return from;
}

template <typename T>
int List<T>::remove(const T& element)
{
	int index = find(element);
	if (index < size())
		pop(index);
	return index;
}

template <typename T>
bool List<T>::contains(const T& element) const
{
	int index = find(element);
	return index < size();
}

template <typename T>
unsigned List<T>::count(const T& element) const
{
	unsigned count = 0;
	for (int from = 0;
		(from = find(element, from) + 1) <= size();
		++count) {}
	return count;
}

} // namespace structures

#endif // STRUCTURES_LIST_H
