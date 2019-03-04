#ifndef STACK_H
#define STACK_H

namespace structures {

template <typename T>
class Stack {
	public:
		Stack(int);
		~Stack(void);

		T pop(void);
		void push(T);

	private:
		T *_elements;
		int _index;
		unsigned int _size;
};

#include "stack.ipp"
// ~.inc~

}

#endif
