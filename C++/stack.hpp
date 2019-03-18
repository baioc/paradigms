#ifndef DATA_STRUCTURES_STACK_H_
#define DATA_STRUCTURES_STACK_H_

#include <cassert>  	// assert
#include <algorithm>	// std::copy

namespace data_structures {


template <typename T>
class Stack {
public:
	explicit Stack(int);
	// rule of three
	~Stack();
	Stack(const Stack& other); // copy constructor
	Stack& operator=(const Stack& other); // copy assignment operator
	// rule of five
	Stack(Stack&& other); // move constructor
	Stack& operator=(Stack&& other); // move assignment operator

	void push(T);
	T pop();
	void pick(int);
	bool empty() const;

	unsigned int size() const
	{
		return allocated_size_; // specific
	}

private:
	T *content_;
	int current_size_;
	unsigned int allocated_size_;
};


template <typename T>
Stack<T>::Stack(int size):
	current_size_(0)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = new T[allocated_size_];
}

template <typename T>
Stack<T>::~Stack()
{
	delete[] content_; // calls destructor of each T
}

template <typename T>
Stack<T>::Stack(const Stack& other):
	current_size_(other.current_size_),
	allocated_size_(other.allocated_size_)
{
	content_ = new T[other.allocated_size_];
	std::copy(other.content_, other.content_ + other.allocated_size_, content_);
}

template <typename T>
Stack<T>& Stack<T>::operator=(const Stack& other)
{
	if (this == &other) // self-assignment guard
		return *this;

	delete[] content_;

	content_ = new T[other.allocated_size_];
	std::copy(other.content_, other.content_ + other.allocated_size_, content_);

	current_size_ = other.current_size_;
	allocated_size_ = other.allocated_size_;
}

template <typename T>
Stack<T>::Stack(Stack&& other)
{
	content_ = other.content_;
	current_size_ = other.current_size_;
	allocated_size_ = other.allocated_size_;

	other.content_ = nullptr;
	other.current_size_ = 0;
	other.allocated_size_ = 0;
}

template <typename T>
Stack<T>& Stack<T>::operator=(Stack&& other)
{
	if (this == &other)
		return *this;

	delete[] content_;
	content_ = other.content_;
	current_size_ = other.current_size_;
	allocated_size_ = other.allocated_size_;

	other.content_ = nullptr;
	other.current_size_ = 0;
	other.allocated_size_ = 0;
}

template <typename T>
void Stack<T>::push(T element)
{
	assert(current_size_ < allocated_size_);
	content_[current_size_++] = element;
}

template <typename T>
T Stack<T>::pop()
{
	assert(current_size_ > 0);
	return content_[--current_size_];
}

template <typename T>
void Stack<T>::pick(int offset)
{
	assert(offset >= 0);
	assert(current_size_ - offset - 1 >= 0);
	push(content_[current_size_ - offset - 1]);
}

template <typename T>
bool Stack<T>::empty() const
{
	return current_size_ <= 0;
}


}	// namespace data_structures

#endif	// ifndef DATA_STRUCTURES_STACK_H_
