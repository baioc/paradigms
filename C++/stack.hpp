#ifndef DATA_STRUCTURES_STACK_H_
#define DATA_STRUCTURES_STACK_H_

#include <cassert>

namespace data_structures {


template <typename T>
class Stack {
public:
	explicit Stack(int);
	~Stack();

	void push(T);
	T pop();
	void pick(int);
	bool empty() const;

	unsigned int size() const
	{
		return _allocated_size; // implementation-specific
	}

private:
	T *_content;
	int _current_size;
	int _allocated_size;
};


template <typename T>
Stack<T>::Stack(int size):
	_current_size(0)
{
	assert(size > 0);
	_allocated_size = size;
	_content = new int[_allocated_size];
}

template <typename T>
Stack<T>::~Stack()
{
	delete[] _content; // calls destructor of each T
}

template <typename T>
void Stack<T>::push(T element)
{
	assert(_current_size < _allocated_size);
	_content[_current_size++] = element;
}

template <typename T>
T Stack<T>::pop()
{
	assert(_current_size > 0);
	return _content[--_current_size];
}

template <typename T>
void Stack<T>::pick(int offset)
{
	assert(offset >= 0);
	assert(_current_size - offset - 1 >= 0);
	push(_content[_current_size - offset - 1]);
}

template <typename T>
bool Stack<T>::empty() const
{
	return _current_size <= 0;
}


}	// namespace data_structures

#endif	// ifndef DATA_STRUCTURES_STACK_H_
