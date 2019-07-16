#ifndef STRUCTURES_ARRAY_LIST_HPP
#define STRUCTURES_ARRAY_LIST_HPP

#include "list.hpp"

#include <algorithm>       	// swap
#include <memory>          	// uninitialized_move, copy
#include <initializer_list>


namespace structures {

template <typename T>
	// requires Sortable<T>
class ArrayList : public structures::List<T> {
 public:
	explicit ArrayList(int size = 16);
	ArrayList(const std::initializer_list<T>& initial);
	~ArrayList();

	ArrayList(const ArrayList& origin);
	ArrayList& operator=(const ArrayList& origin);
	ArrayList(ArrayList&& source);
	ArrayList& operator=(ArrayList&& source);
	ArrayList& operator+=(const ArrayList& rhs);

	int insert(T element); //! sorted insertion
	void sort();

	T& operator[](int index);
	const T& operator[](int index) const;

	void insert(int index, T element);
	T pop(int index);

	using List<T>::push_back;
	using List<T>::pop_back;
	using List<T>::back;
	using List<T>::push_front;
	using List<T>::pop_front;
	using List<T>::front;

	int size() const;
	using List<T>::empty;

	//! when find() fails, it returns a number equal to size()
	using List<T>::find;
	using List<T>::remove;
    using List<T>::contains;
	using List<T>::count;

 private:
	T* content_{nullptr};
	int tail_{-1};
	int allocated_size_{0};

	int insertion(int, T);
	void grow(float = 2.0);

	friend void swap(ArrayList<T>& a, ArrayList<T>& b)
	{
		using std::swap;
		swap(a.content_, b.content_);
		swap(a.tail_, b.tail_);
		swap(a.allocated_size_, b.allocated_size_);
	}
};

template <typename T>
ArrayList<T> operator+(ArrayList<T> lhs, const ArrayList<T>& rhs)
{
	lhs += rhs;
	return lhs;
}

} // structures


#include <cassert>
#include <iterator>	// make_move_iterator

namespace structures {

template <typename T>
ArrayList<T>::ArrayList(int size)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = static_cast<T*>(malloc(sizeof(T) * size));
	assert(content_ != nullptr);
}

template <typename T>
ArrayList<T>::~ArrayList()
{
	free(content_);
}

template <typename T>
ArrayList<T>::ArrayList(const std::initializer_list<T>& initial):
	ArrayList(initial.size())
{
	auto data = content_;
	for (auto&& element : initial) {
		new (data) T(element);
		data++;
		++tail_;
	}
}

template <typename T>
ArrayList<T>::ArrayList(const ArrayList<T>& origin):
	tail_{origin.tail_},
	allocated_size_{origin.allocated_size_}
{
	content_ = static_cast<T*>(malloc(sizeof(T) * origin.allocated_size_));
	assert(content_ != nullptr);
	std::uninitialized_copy(origin.content_, origin.content_ + origin.allocated_size_, content_);
}

template <typename T>
ArrayList<T>& ArrayList<T>::operator=(const ArrayList<T>& origin)
{
	ArrayList temp(origin);
	swap(*this, temp);
	return *this;
}

template <typename T>
ArrayList<T>::ArrayList(ArrayList<T>&& source):
	ArrayList()
{
	swap(*this, source);
}

template <typename T>
ArrayList<T>& ArrayList<T>::operator=(ArrayList<T>&& source)
{
	swap(*this, source);
	return *this;
}

template <typename T>
inline int ArrayList<T>::size() const
{
	return tail_ + 1;
}

template <typename T>
inline T& ArrayList<T>::operator[](int index)
{
	assert(index >= 0);
	assert(index < size());
	return content_[index];
}

template <typename T>
inline const T& ArrayList<T>::operator[](int index) const
{
	assert(index >= 0);
	assert(index < size());
	return content_[index];
}

template <typename T>
void ArrayList<T>::grow(float scaling)
{
	assert(allocated_size_ > 0);
	allocated_size_ *= scaling;

	auto mem = static_cast<T*>(realloc(content_, sizeof(T) * allocated_size_));
	if (mem == nullptr) {
		free(content_);
		exit(ENOMEM);
	}

	content_ = mem;
}

template <typename T>
void ArrayList<T>::insert(int position, T element)
{
	assert(position >= 0);
	assert(position <= size());
	if (size() >= allocated_size_)
		grow();

	using std::swap;
	for (int i = size(); i > position; --i)
		swap(content_[i], content_[i-1]);

	new (content_ + position) T(std::move(element));
	tail_++;
}

template <typename T>
T ArrayList<T>::pop(int position)
{
	assert(position >= 0);
	assert(position < size());

	const auto target = std::move(content_[position]);

	using std::swap;
	for (int i = position; i < tail_; ++i)
		swap(content_[i], content_[i+1]);

	tail_--;
	return target;
}

template <typename T>
int ArrayList<T>::insertion(int end, T element)
{
	int i = end;
	using std::swap;
	for (; i >= 0 && content_[i] > element; --i)
		swap(content_[i+1], content_[i]);

	const int pos = i+1;
	new (content_ + pos) T(std::move(element));

	return pos;
}

template <typename T>
int ArrayList<T>::insert(T element)
{
	if (size() >= allocated_size_)
		grow();

	return insertion(tail_++, std::move(element));
}

template <typename T>
void ArrayList<T>::sort()
{
	// Insertion Sort
	for (int i = 1; i < size(); ++i)
		insertion(i - 1, std::move(content_[i]));
}

template <typename T>
ArrayList<T>& ArrayList<T>::operator+=(const ArrayList<T>& rhs)
{
	const auto new_allocated_size = allocated_size_ + rhs.allocated_size_;
	auto new_content = static_cast<T*>(malloc(sizeof(T) * new_allocated_size));
	assert(new_content != nullptr);

	std::uninitialized_move(content_, content_+size(), new_content);
	std::uninitialized_copy(rhs.content_, rhs.content_+rhs.size(), new_content+size());

	std::swap(content_, new_content);
	free(new_content);

	tail_ += rhs.size();
	allocated_size_ = new_allocated_size;

	return *this;
}

} // structures

#endif // STRUCTURES_ARRAY_LIST_HPP
