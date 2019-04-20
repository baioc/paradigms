#ifndef BAIOC_ARRAY_LIST_HPP
#define BAIOC_ARRAY_LIST_HPP

#include <algorithm>       	// std::copy, swap
#include <initializer_list>

#include "list.hpp"


namespace baioc {

template <typename T>
	// requires Sortable<T>
class ArrayList : public baioc::List<T> {
 public:
	explicit ArrayList(int size = 16);
	ArrayList(std::initializer_list<T> initial);
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

	void insert(int index, T);
	T pop(int index);

	int size() const;
	using List<T>::empty;

 private:
	T* content_{nullptr};
	int tail_{-1};
	int allocated_size_{0};

	int insertion(int, T);
	void grow(float = 2.0);

	friend void swap(ArrayList<T>& a, ArrayList<T>& b) {
		using std::swap;
		swap(a.content_, b.content_);
		swap(a.tail_, b.tail_);
		swap(a.allocated_size_, b.allocated_size_);
	}
};

template <typename T>
ArrayList<T> operator+(ArrayList<T> lhs, const ArrayList<T>& rhs) {
	lhs += rhs;
	return lhs;
}

} // baioc


#include <cassert>
#include <iterator>	// make_move_iterator

namespace baioc {

template <typename T>
ArrayList<T>::ArrayList(int size) {
	assert(size > 0);
	allocated_size_ = size;
	content_ = static_cast<T*>(malloc(sizeof(T) * size));
	assert(content_ != nullptr);
}

template <typename T>
ArrayList<T>::~ArrayList() {
	free(content_);
}

template <typename T>
ArrayList<T>::ArrayList(std::initializer_list<T> initial):
	ArrayList(initial.size())
{
	auto data = content_;
	for (auto&& element : initial) {
		*(data++) = element;
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
	std::copy(origin.content_, origin.content_ + origin.allocated_size_, content_);
}

template <typename T>
ArrayList<T>& ArrayList<T>::operator=(const ArrayList<T>& origin) {
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
ArrayList<T>& ArrayList<T>::operator=(ArrayList<T>&& source) {
	swap(*this, source);
	return *this;
}

template <typename T>
inline int ArrayList<T>::size() const {
	return tail_ + 1;
}

template <typename T>
T& ArrayList<T>::operator[](int index) {
	assert(index >= 0);
	assert(index < size());
	return content_[index];
}

template <typename T>
const T& ArrayList<T>::operator[](int index) const {
	assert(index >= 0);
	assert(index < size());
	return content_[index];
}

template <typename T>
void ArrayList<T>::grow(float scaling) {
	assert(allocated_size_ * scaling >= 1);
	allocated_size_ *= scaling;
	content_ = static_cast<T*>(realloc(content_, sizeof(T) * allocated_size_));
	assert(content_ != nullptr);
}

template <typename T>
void ArrayList<T>::insert(int position, T element) {
	assert(position >= 0);
	assert(position <= size());
	if (size() >= allocated_size_)
		grow();

	using std::swap;
	for (int i = size(); i > position; --i)
		swap(content_[i], content_[i-1]);

	content_[position] = std::move(element);
	tail_++;
}

template <typename T>
T ArrayList<T>::pop(int position) {
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
int ArrayList<T>::insertion(int end, T element) {
	int i = end;
	using std::swap;
	for (; i >= 0 && content_[i] > element; --i)
		swap(content_[i+1], content_[i]);

	const int pos = i+1;
	content_[pos] = std::move(element);
	return pos;
}

template <typename T>
int ArrayList<T>::insert(T element) {
	if (size() >= allocated_size_)
		grow();

	return insertion(tail_++, element);
}

template <typename T>
void ArrayList<T>::sort() {
	// Insertion Sort
	for (int i = 1; i < size(); ++i)
		insertion(i - 1, content_[i]);
}

template <typename T>
ArrayList<T>& ArrayList<T>::operator+=(const ArrayList<T>& rhs) {
	const auto new_allocated_size = allocated_size_ + rhs.allocated_size_;
	auto new_content = static_cast<T*>(malloc(sizeof(T) * new_allocated_size));
	assert(new_content != nullptr);

	for (int i = 0; i < size(); ++i)
		new_content[i] = std::move(content_[i]);

	for (int i = 0; i < rhs.size(); ++i) {
		T temp(rhs.content_[i]);
		new_content[i + size()] = std::move(temp);
	}

	std::swap(content_, new_content);
	free(new_content);

	tail_ += rhs.size();
	allocated_size_ = new_allocated_size;

	return *this;
}

} // baioc

#endif // BAIOC_ARRAY_LIST_HPP
