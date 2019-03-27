#ifndef STRUCTURES_STACK_HPP
#define STRUCTURES_STACK_HPP

#include <cstdint>  	// std::size_t
#include <algorithm>	// std::swap, std::copy

#include <cassert>
#include <stdexcept>	// C++ exceptions


namespace structures {

template <typename T>
class Stack {
 public:
	explicit Stack(int);
	Stack(); // uses DEFAULT_SIZE
	// rule of three
	~Stack();
	Stack(const Stack&); // copy constructor
	Stack& operator=(Stack); // copy assignment operator
	// rule of five
	Stack(Stack&&); // move constructor
	Stack& operator=(Stack&&) noexcept; // move assignment operator

	void push(const T&);
	T pop();
	T& top(); //! returns ref to internal dinamically allocated memory
	bool empty() const;
	std::size_t size() const;
	void clear(); //! DO NOT use if T is a raw pointer, memory WILL LEAK
	void pick(int);

 private:
	static const auto DEFAULT_SIZE = 8u;

	T *content_;
	std::size_t current_size_;
	std::size_t allocated_size_;

	bool full() const;

	// rule of three/five and a half
	friend void swap(Stack<T>& a, Stack<T>& b)
	{
		using std::swap; // enables ADL
		swap(a.content_, b.content_);
		swap(a.current_size_, b.current_size_);
		swap(a.allocated_size_, b.allocated_size_);
	}
};

}	// namespace structures


template <typename T>
structures::Stack<T>::Stack(int size):
	current_size_(0)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = new T[allocated_size_];
}

template <typename T>
structures::Stack<T>::Stack():
	Stack(DEFAULT_SIZE)
{}

template <typename T>
structures::Stack<T>::~Stack()
{
	delete[] content_;	// frees each individual object
}

template <typename T>
structures::Stack<T>::Stack(const Stack& origin):
	current_size_(origin.current_size_),
	allocated_size_(origin.allocated_size_)
{
	content_ = new T[origin.allocated_size_];
	std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);
}

template <typename T>
structures::Stack<T>& structures::Stack<T>::operator=(Stack origin)
// if you're going to make a copy of something in a function,
// then let the compiler do it in the parameter list (uses copy constructor)
{
	swap(*this, origin);
    return *this;
}

template <typename T>
structures::Stack<T>::Stack(Stack&& other):
	Stack()
{
	swap(*this, other);
}

template <typename T>
structures::Stack<T>& structures::Stack<T>::operator=(Stack&& other) noexcept
{
	swap(*this, other);
	return *this;
}

template <typename T>
// element may bind to either lvalue or rvalue reference
void structures::Stack<T>::push(const T& element)
{
	if (full())
		throw std::out_of_range("Stack overflow.");

	content_[current_size_++] = element;
}

template <typename T>
T structures::Stack<T>::pop()
{
	if (empty())
		throw std::out_of_range("Stack underflow.");

	return content_[--current_size_];
}

template <typename T>
T& structures::Stack<T>::top()
{
	if (empty())
		throw std::out_of_range("Stack has nothing on top.");

	return content_[current_size_ - 1];
}

template <typename T>
inline bool structures::Stack<T>::empty() const
{
	return !(current_size_ > 0);
}

template <typename T>
inline bool structures::Stack<T>::full() const
{
	return !(current_size_ < allocated_size_);
}

template <typename T>
inline std::size_t structures::Stack<T>::size() const
{
	return current_size_;
}

template <typename T>
inline void structures::Stack<T>::clear()
{
	current_size_ = 0;
}

template <typename T>
void structures::Stack<T>::pick(int offset)
{
	assert(offset >= 0);
	int access = current_size_ - offset - 1;
	if (access < 0)
		throw std::out_of_range("Can't pick that far down the stack.");

	push(content_[access]);
}

#endif	// STRUCTURES_STACK_HPP
