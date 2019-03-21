#ifndef STRUCTURES_ARRAY_STACK_H
#define STRUCTURES_ARRAY_STACK_H

#include <cstdint>  	// std::size_t
#include <algorithm>	// std::swap

namespace structures {

template <typename T>
class ArrayStack {
 public:
	ArrayStack(); // uses DEFAULT_SIZE
	explicit ArrayStack(int);
	// rule of three
	~ArrayStack();
	ArrayStack(const ArrayStack&); // copy constructor
	ArrayStack& operator=(ArrayStack); // copy assignment operator
	// rule of five
	ArrayStack(ArrayStack&&); // move constructor
	ArrayStack& operator=(ArrayStack&&) noexcept; // move assignment operator

	void push(const T&);
	T pop();
	T& top();
	std::size_t size() const;
	std::size_t max_size() const;
	bool empty() const;
	bool full() const;
	void clear();
	void pick(int);

 private:
	static const auto DEFAULT_SIZE = 8u;

	// rule of three/five and a half
	friend void swap(ArrayStack<T>& a, ArrayStack<T>& b)
	{
		using std::swap; // enables ADL
		swap(a.content_, b.content_);
		swap(a.current_size_, b.current_size_);
		swap(a.allocated_size_, b.allocated_size_);
	}

	T *content_;
	std::size_t current_size_;
	std::size_t allocated_size_;
};

}	// namespace structures

#endif	// STRUCTURES_ARRAY_STACK_H


#include <cstdint>  	// std::size_t
#include <stdexcept>	// C++ exceptions
#include <algorithm>	// std::copy
#include <cassert>

using namespace structures;

template <typename T>
ArrayStack<T>::ArrayStack(int size):
	current_size_(0)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = new T[allocated_size_];
}

template <typename T>
ArrayStack<T>::ArrayStack():
	ArrayStack(DEFAULT_SIZE)
{}

template <typename T>
ArrayStack<T>::~ArrayStack()
{
	delete[] content_;	// frees each individual object
}

template <typename T>
ArrayStack<T>::ArrayStack(const ArrayStack& origin):
	current_size_(origin.current_size_),
	allocated_size_(origin.allocated_size_)
{
	content_ = new T[origin.allocated_size_];
	std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);
}

template <typename T>
ArrayStack<T>& ArrayStack<T>::operator=(ArrayStack origin)
// if you're going to make a copy of something in a function,
// then let the compiler do it in the parameter list (uses copy constructor)
{
	swap(*this, origin);
    return *this;
}

template <typename T>
ArrayStack<T>::ArrayStack(ArrayStack&& other):
	ArrayStack()
{
	swap(*this, other);
}

template <typename T>
ArrayStack<T>& ArrayStack<T>::operator=(ArrayStack&& other) noexcept
{
	swap(*this, other);
	return *this;
}

template <typename T>
// element may bind to either lvalue or rvalue reference
void ArrayStack<T>::push(const T& element)
{
	if (full())
		throw std::out_of_range("Stack overflow.");

	content_[current_size_++] = element;
}

template <typename T>
T ArrayStack<T>::pop()
{
	if (empty())
		throw std::out_of_range("Stack underflow.");

	return content_[--current_size_];
}

template <typename T>
T& ArrayStack<T>::top()
{
	if (empty())
		throw std::out_of_range("Stack has nothing on top.");

	return content_[current_size_ - 1];
}

template <typename T>
inline std::size_t ArrayStack<T>::size() const
{
	return current_size_;
}

template <typename T>
inline std::size_t ArrayStack<T>::max_size() const
{
	return allocated_size_;
}

template <typename T>
inline bool ArrayStack<T>::empty() const
{
	return !(current_size_ > 0);
}

template <typename T>
inline bool ArrayStack<T>::full() const
{
	return !(current_size_ < allocated_size_);
}

template <typename T>
void ArrayStack<T>::clear()
{
	current_size_ = 0;
}

template <typename T>
void ArrayStack<T>::pick(int offset)
{
	assert(offset >= 0);

	int access = current_size_ - offset - 1;
	if (access < 0)
		throw std::out_of_range("Can't pick that far down the stack.");

	push(content_[access]);
}
