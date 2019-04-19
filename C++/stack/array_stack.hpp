#ifndef STRUCTURES_STACK_HPP
#define STRUCTURES_STACK_HPP

#include <algorithm>	// std::swap, std::copy

#include <cassert>


namespace structures {

template <typename T>
	// requires MoveAssignable<T>
class Stack {
 public:
	explicit Stack(int = DEFAULT_SIZE_);
	// rule of three
	~Stack();
	Stack(const Stack&);
	Stack& operator=(const Stack&);
	// rule of five
	Stack(Stack&&);
	Stack& operator=(Stack&&);

	void push(T);
	T pop();
	T& top(); //! returns ref to internal dinamically allocated memory
	bool empty() const;
	int size() const;
	void pick(int);

	//! DO NOT use if T is a raw pointer, WILL LEAK MEMORY
	void clear()
	{
		current_size_ = 0;
	}

 private:
	static const auto DEFAULT_SIZE_ = 8;

	T *content_{nullptr};
	int current_size_{0};
	int allocated_size_{0};

	void grow(float = 2.0);

	// rule of three/five and a half
	friend void swap(Stack<T>& a, Stack<T>& b)
	{
		using std::swap; // enables ADL
		swap(a.content_, b.content_);
		swap(a.current_size_, b.current_size_);
		swap(a.allocated_size_, b.allocated_size_);
	}
};


template <typename T>
Stack<T>::Stack(int size)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = static_cast<T*>(malloc(sizeof(T) * size));
	assert(content_ != nullptr);
}

template <typename T>
Stack<T>::~Stack()
{
	free(content_);
}

template <typename T>
Stack<T>::Stack(const Stack<T>& origin):
	current_size_{origin.current_size_},
	allocated_size_{origin.allocated_size_}
{
	content_ = static_cast<T*>(malloc(sizeof(T) * origin.allocated_size_));
	assert(content_ != nullptr);
	std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);
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
void Stack<T>::grow(float scaling)
{
	assert(allocated_size_ * scaling >= 1);
	allocated_size_ *= scaling;
	content_ = static_cast<T*>(realloc(content_, sizeof(T) * allocated_size_));
	assert(content_ != nullptr);
}

template <typename T>
void Stack<T>::push(T element)
// if you're going to make a copy of something in a function,
// then let the compiler do it in the parameter list.
// element may bind to either lvalue (uses copy constructor) or rvalue reference
{
	if (current_size_ >= allocated_size_)
		grow();

	content_[current_size_++] = std::move(element);
}

template <typename T>
T Stack<T>::pop()
{
	assert(!empty());
	return content_[--current_size_];
}

template <typename T>
T& Stack<T>::top()
{
	assert(!empty());
	return content_[current_size_ - 1];
}

template <typename T>
inline bool Stack<T>::empty() const
{
	return current_size_ <= 0;
}

template <typename T>
inline int Stack<T>::size() const
{
	return current_size_;
}

template <typename T>
void Stack<T>::pick(int offset)
{
	assert(offset >= 0);
	int access = current_size_ - offset - 1;
	assert(access >= 0);
	push(content_[access]);
}

} // namespace structures

#endif // STRUCTURES_STACK_HPP
