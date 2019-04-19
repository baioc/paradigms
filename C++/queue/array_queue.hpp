#ifndef STRUCTURES_QUEUE_HPP
#define STRUCTURES_QUEUE_HPP

#include <memory>   	// unique_ptr

namespace structures {

template <typename T>
	// requires MoveAssignable<T>
class Queue {
 public:
	explicit Queue(int = DEFAULT_SIZE_);

	// can't be copied because of the unique_ptr

	void enqueue(T);
	T dequeue();
	T& front();
	T& back();
	bool empty() const;
	int size() const;

	//! DO NOT use if T is a raw pointer, WILL LEAK MEMORY
	void clear()
	{
		current_size_ = 0;
	}

 private:
	static const auto DEFAULT_SIZE_ = 8;

	std::unique_ptr<T[]> content_;
	int current_size_{0};
	int allocated_size_{0};
	int front_{0};
	int back_{0};

	bool full() const;
};

} // namespace structures


#include <cassert>
#include <stdexcept>	// exceptions

namespace structures {

template <typename T>
Queue<T>::Queue(int size)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = std::unique_ptr<T[]>(new T[allocated_size_]);
}

template <typename T>
void Queue<T>::enqueue(T data)
{
	if (full())
		throw std::out_of_range("Queue overflow.");

	content_[back_] = std::move(data);
	back_ = (back_ + 1) % allocated_size_;
	current_size_++;
}

template <typename T>
T Queue<T>::dequeue()
{
	if (empty())
		throw std::out_of_range("Queue underflow.");

	const auto take_idx = front_;
	front_ = (front_ + 1) % allocated_size_;
	current_size_--;
	return content_[take_idx];
}

template <typename T>
T& Queue<T>::front()
{
	if (empty())
		throw std::out_of_range("Queue is empty.");

	return content_[front_];
}

template <typename T>
T& Queue<T>::back()
{
	if (empty())
		throw std::out_of_range("Queue is empty.");

	return content_[back_ == 0 ? allocated_size_ - 1 : back_ - 1];
}

template <typename T>
inline bool Queue<T>::empty() const
{
	return current_size_ <= 0;
}

template <typename T>
inline bool Queue<T>::full() const
{
	return current_size_ >= allocated_size_;
}

template <typename T>
inline int Queue<T>::size() const
{
	return current_size_;
}

} // namespace structures

#endif // STRUCTURES_QUEUE_HPP
