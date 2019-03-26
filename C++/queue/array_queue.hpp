#ifndef STRUCTURES_QUEUE_HPP
#define STRUCTURES_QUEUE_HPP

#include <cstdint>  	// std::size_t
#include <algorithm>	// std::swap, std::copy

#include <cassert>
#include <stdexcept>	// exceptions


namespace structures {

template <typename T>
class Queue {
 public:
	explicit Queue(int);
	Queue();

	~Queue();
	Queue(const Queue&);
	Queue& operator=(Queue);
	Queue(Queue&&);
	Queue& operator=(Queue&&) noexcept;

	void enqueue(const T&);
	T dequeue();
	T& back();
	bool empty() const;
	bool full() const;
	std::size_t size() const;
	void clear(); //! DO NOT use if T is a raw pointer, WILL LEAK MEMORY

 private:
	static const auto DEFAULT_SIZE = 8u;

	T *content_;
	std::size_t current_size_;
	std::size_t allocated_size_;
	unsigned front_;
	unsigned back_;

	friend void swap(Queue<T>& a, Queue<T>& b)
	{
		using std::swap;
		swap(a.content_, b.content_);
		swap(a.current_size_, b.current_size_);
		swap(a.allocated_size_, b.allocated_size_);
		swap(a.front_, b.front_);
		swap(a.back_, b.back_);
	}
};

}	// namespace structures


template <typename T>
structures::Queue<T>::Queue(int size):
	current_size_(0),
	front_(0),
	back_(0)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = new T[allocated_size_];
}

template <typename T>
structures::Queue<T>::Queue():
	Queue(DEFAULT_SIZE)
{}

template <typename T>
structures::Queue<T>::~Queue()
{
	delete[] content_;
}

template <typename T>
structures::Queue<T>::Queue(const Queue& origin):
	current_size_(origin.current_size_),
	allocated_size_(origin.allocated_size_),
	front_(origin.front_),
	back_(origin.back_)
{
	content_ = new T[origin.allocated_size_];
	std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);
}

template <typename T>
structures::Queue<T>& structures::Queue<T>::operator=(Queue origin)
{
	swap(*this, origin);
	return *this;
}

template <typename T>
structures::Queue<T>::Queue(Queue&& other):
	Queue()
{
	swap(*this, other);
}

template <typename T>
structures::Queue<T>& structures::Queue<T>::operator=(Queue&& other) noexcept
{
	swap(*this, other);
	return *this;
}

template <typename T>
void structures::Queue<T>::enqueue(const T& data)
{
	if (full())
		throw std::out_of_range("Queue overflow.");

	content_[back_] = data;
	back_ = (back_ + 1) % allocated_size_;
	current_size_++;
}

template <typename T>
T structures::Queue<T>::dequeue()
{
	if (empty())
		throw std::out_of_range("Queue underflow.");

	auto take_idx = front_;
	front_ = (front_ + 1) % allocated_size_;
	current_size_--;
	return content_[take_idx];
}

template <typename T>
T& structures::Queue<T>::back()
{
	if (empty())
		throw std::out_of_range("Queue is empty.");

	auto last_back = back_ == 0 ? allocated_size_ - 1 : back_ - 1;
	return content_[last_back];
}

template <typename T>
inline bool structures::Queue<T>::empty() const
{
	return current_size_ <= 0;
}

template <typename T>
inline bool structures::Queue<T>::full() const
{
	return current_size_ >= allocated_size_;
}

template <typename T>
inline std::size_t structures::Queue<T>::size() const
{
	return current_size_;
}

template <typename T>
inline void structures::Queue<T>::clear()
{
	current_size_ = 0;
}

#endif // STRUCTURES_QUEUE_HPP
