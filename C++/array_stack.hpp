// Copyright [2019] <Gabriel Baiocchi de Sant'Anna>
#ifndef STRUCTURES_ARRAY_STACK_H
#define STRUCTURES_ARRAY_STACK_H

#include <cstdint>  	// std::size_t

namespace structures {

template <typename T>
class ArrayStack {
public:
	ArrayStack(); // uses DEFAULT_SIZE
	explicit ArrayStack(int);
	// rule of three
	~ArrayStack();
	ArrayStack(const ArrayStack&); // copy constructor
	ArrayStack& operator=(const ArrayStack&); // copy assignment operator
	// rule of five
	ArrayStack(ArrayStack&&); // move constructor
	ArrayStack& operator=(ArrayStack&&); // move assignment operator

	void push(const T&);
	T pop();
	T& top() const;
	std::size_t size() const;
	std::size_t max_size() const;
	bool empty() const;
	bool full() const;
	void clear();
	void pick(int);

private:
	T *content_;
	std::size_t current_size_;
	std::size_t allocated_size_;

	static const auto DEFAULT_SIZE = 32u;
};

}	// namespace structures

#endif	// ifndef STRUCTURES_ARRAY_STACK_H


#include <stdexcept>	// C++ exceptions
#include <cassert>

template <typename T>
structures::ArrayStack<T>::ArrayStack(int size):
	current_size_(0)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = new T[allocated_size_];
}

template <typename T>
structures::ArrayStack<T>::ArrayStack()
{
	ArrayStack(DEFAULT_SIZE);
}

template <typename T>
structures::ArrayStack<T>::~ArrayStack()
{
	delete[] content_;	// individual objects are NOT freed
}

template <typename T>
structures::ArrayStack<T>::ArrayStack(const ArrayStack& origin)
{
	content_ = new T[origin.allocated_size_];
	std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);

	current_size_ = origin.current_size_;
	allocated_size_ = origin.allocated_size_;
}

template <typename T>
structures::ArrayStack<T>& structures::ArrayStack<T>::operator=(const ArrayStack& origin)
{
	if (this == &origin) // self-assignment guard
		return *this;

	{ // destructor
		delete[] content_;
	}

	{ // copy constructor
		content_ = new T[origin.allocated_size_];
		std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);

		current_size_ = origin.current_size_;
		allocated_size_ = origin.allocated_size_;
	}

	return *this;
}

template <typename T>
structures::ArrayStack<T>::ArrayStack(ArrayStack&& other)
{
	content_ = other.content_;
	current_size_ = other.current_size_;
	allocated_size_ = other.allocated_size_;

	other.content_ = nullptr;
	other.current_size_ = 0;
	other.allocated_size_ = 0;
}

template <typename T>
structures::ArrayStack<T>& structures::ArrayStack<T>::operator=(ArrayStack&& other)
{
	if (this == &other)
		return *this;

	{ // destructor
		delete[] content_;
	}

	{ // move constructor
		content_ = other.content_;
		current_size_ = other.current_size_;
		allocated_size_ = other.allocated_size_;

		other.content_ = nullptr;
		other.current_size_ = 0;
		other.allocated_size_ = 0;
	}

	return *this;
}

template <typename T>
void structures::ArrayStack<T>::push(const T& element)
{
	if (full())
		throw std::out_of_range("Stack overflow.");

	content_[current_size_++] = element;
}

template <typename T>
T structures::ArrayStack<T>::pop()
{
	if (empty())
		throw std::out_of_range("Stack underflow.");

	return content_[--current_size_];
}

template <typename T>
T& structures::ArrayStack<T>::top() const
{
	if (empty())
		throw std::out_of_range("Stack has nothing on top.");

	return content_[current_size_ - 1];
}

template <typename T>
inline std::size_t structures::ArrayStack<T>::size() const
{
	return current_size_;
}

template <typename T>
inline std::size_t structures::ArrayStack<T>::max_size() const
{
	return allocated_size_;
}

template <typename T>
inline bool structures::ArrayStack<T>::empty() const
{
	return !(current_size_ > 0);
}

template <typename T>
inline bool structures::ArrayStack<T>::full() const
{
	return !(current_size_ < allocated_size_);
}

template <typename T>
void structures::ArrayStack<T>::clear()
{
	current_size_ = 0;
}

template <typename T>
void structures::ArrayStack<T>::pick(int offset)
{
	assert(offset >= 0);

	int access = current_size_ - offset - 1;
	if (access < 0)
		throw std::out_of_range("Can't pick that far down the stack.");

	push(content_[access]);
}
