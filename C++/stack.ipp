#include <assert.h>

template <typename T>
Stack<T>::Stack(int size)
	: _index(0),
	  _size(size) {
	assert(size > 0);
	_elements = new int [_size];
}

template <typename T>
Stack<T>::~Stack(void) {
	delete [] _elements;
}

template <typename T>
T Stack<T>::pop(void) {
	assert(_index > 0);
	return _elements[--_index];
}

template <typename T>
void Stack<T>::push(T element) {
	assert(_index < _size);
	_elements[_index++] = element;
}
