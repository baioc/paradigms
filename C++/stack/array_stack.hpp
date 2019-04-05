#ifndef STRUCTURES_STACK_HPP
#define STRUCTURES_STACK_HPP

#include <cstdint>  	// std::size_t
#include <algorithm>	// std::swap, std::copy
#include <memory>   	// unique_ptr

namespace structures {

template <typename T>
	// requires MoveAssignable<T>
class Stack {
 public:
	explicit Stack(int);
	Stack(): Stack(DEFAULT_SIZE_) {}
	// rule of three
	~Stack() = default;
	Stack(const Stack&);
	Stack& operator=(const Stack&);
	// rule of five
	Stack(Stack&&);
	Stack& operator=(Stack&&);

	void push(T);
	T pop();
	T& top(); //! returns ref to internal dinamically allocated memory
	bool empty() const;
	std::size_t size() const;
	void pick(int);

	//! DO NOT use if T is a raw pointer, WILL LEAK MEMORY
	void clear()
	{
		current_size_ = 0;
	}

 private:
	static const auto DEFAULT_SIZE_ = 8u;

	std::unique_ptr<T[]> content_;
	std::size_t current_size_{0};
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


#include <cassert>
#include <stdexcept>	// C++ exceptions


namespace structures {

template <typename T>
Stack<T>::Stack(int size)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = std::unique_ptr<T[]>(new T[allocated_size_]);
}

template <typename T>
Stack<T>::Stack(const Stack<T>& origin):
	current_size_(origin.current_size_),
	allocated_size_(origin.allocated_size_)
{
	content_ = std::unique_ptr<T[]>(new T[origin.allocated_size_]);
	std::copy(origin.content_.get(), origin.content_.get() + origin.allocated_size_, content_.get());
}

template <typename T>
Stack<T>& Stack<T>::operator=(const Stack<T>& origin)
{
	Stack temp(origin);
	swap(*this, temp);
    return *this;
}

template <typename T>
Stack<T>::Stack(Stack<T>&& other):
	Stack()
{
	swap(*this, other);
}

template <typename T>
Stack<T>& Stack<T>::operator=(Stack<T>&& other)
{
	swap(*this, other);
	return *this;
}

template <typename T>
void Stack<T>::push(T element)
// if you're going to make a copy of something in a function,
// then let the compiler do it in the parameter list.
// element may bind to either lvalue (uses copy constructor) or rvalue reference
{
	if (full())
		throw std::out_of_range("Stack overflow.");

	content_[current_size_++] = std::move(element);
}

template <typename T>
T Stack<T>::pop()
{
	if (empty())
		throw std::out_of_range("Stack underflow.");

	return content_[--current_size_];
}

template <typename T>
T& Stack<T>::top()
{
	if (empty())
		throw std::out_of_range("Stack has nothing on top.");

	return content_[current_size_ - 1];
}

template <typename T>
inline bool Stack<T>::empty() const
{
	return !(current_size_ > 0);
}

template <typename T>
inline bool Stack<T>::full() const
{
	return !(current_size_ < allocated_size_);
}

template <typename T>
inline std::size_t Stack<T>::size() const
{
	return current_size_;
}

template <typename T>
void Stack<T>::pick(int offset)
{
	assert(offset >= 0);
	int access = current_size_ - offset - 1;
	if (access < 0)
		throw std::out_of_range("Can't pick that far down the stack.");

	push(content_[access]);
}

} // namespace structures

#endif // STRUCTURES_STACK_HPP
