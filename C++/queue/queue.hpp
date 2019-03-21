#ifndef STRUCTURES_QUEUE_HPP
#define STRUCTURES_QUEUE_HPP

#include <cstdint>  	// std::size_t
#include <algorithm>	// std::swap

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
	Queue& operator=(Queue&&);

	void enqueue(const T&);
	T dequeue();
	T& back();
	bool empty() const;
	bool full() const;
	std::size_t size() const;
	void clear();

 private:
	static const auto DEFAULT_SIZE = 8u;

	T *content_;
	std::size_t current_size_;
	std::size_t allocated_size_;
	int front_;
	int back_;

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


// TODO

#endif // STRUCTURES_QUEUE_HPP
