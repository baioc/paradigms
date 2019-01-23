template <typename T>
Stack<T>::Stack(int size) : _index(0), _size(size)
{
	_elements = new int [size];
}

template <typename T>
Stack<T>::~Stack(void)
{
	delete [] _elements;
}

template <typename T>
T Stack<T>::pop(void)
{
	if (_index > 0)
		return _elements[--_index];
	else
		return _elements[0];	// @TODO
}

template <typename T>
void Stack<T>::push(const T value)
{
	if (_index < _size)
		_elements[_index++] = value;
	else
		return;	// @TODO
}
