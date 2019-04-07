#ifndef STRUCTURES_ARRAY_LIST_HPP
#define STRUCTURES_ARRAY_LIST_HPP

#include <memory>          	// unique_ptr
#include <initializer_list>
#include <cstdint>         	// size_t
#include <algorithm>       	// std::copy, swap

namespace structures {

template <typename T>
	// requires MoveAssignable<T>
	// && Sortable<T>
	// && Equality_comparable<T>
	// && CopyConstructible<T>
class ArrayList {
 public:
	explicit ArrayList(int);
	ArrayList(): ArrayList(DEFAULT_SIZE_) {}
	ArrayList(std::initializer_list<T>);

	ArrayList(const ArrayList&);
	ArrayList& operator=(const ArrayList&);
	ArrayList(ArrayList&&);
	ArrayList& operator=(ArrayList&&);

	T& operator[](int);
	const T& operator[](int) const;
	ArrayList& operator+=(const ArrayList&);

	void insert(int, T);
	T pop(int);

	void push_back(T);
	T pop_back();
	T& back();
	void push_front(T);
	T pop_front();
	T& front();

	bool empty() const;
	std::size_t size() const;

	int insert(T); //! sorted insertion
	void sort();

	//! when find() fails, it returns a number equal to size()
	int find(const T&, int=0) const;
	int remove(const T&);
    bool contains(const T&) const;
	unsigned count(const T&) const;

 private:
	static const auto DEFAULT_SIZE_ = 16u;

	std::unique_ptr<T[]> content_;
	int tail_{-1};
	std::size_t allocated_size_;

	bool full() const;
	int insertion(int, T);

	friend void swap(ArrayList<T>& a, ArrayList<T>& b)
	{
		using std::swap;
		swap(a.content_, b.content_);
		swap(a.tail_, b.tail_);
		swap(a.allocated_size_, b.allocated_size_);
	}
};

template <typename T>
ArrayList<T> operator+(ArrayList<T>, const ArrayList<T>&);

} // namespace structures


#include <cassert>
#include <iterator> 	// make_move_iterator
#include <stdexcept>	// C++ exceptions

namespace structures {

template <typename T>
ArrayList<T>::ArrayList(int size)
{
	assert(size > 0);
	allocated_size_ = size;
	content_ = std::unique_ptr<T[]>(new T[allocated_size_]);
}

template <typename T>
ArrayList<T>::ArrayList(std::initializer_list<T> init):
	ArrayList(init.size())
{
	auto it = std::make_move_iterator(init.begin());
	const auto ul = std::make_move_iterator(init.end());
	auto data = content_.get();

	while (it != ul) {
		*(data++) = *(it++);
		++tail_;
	}
}

template <typename T>
ArrayList<T>::ArrayList(const ArrayList<T>& origin):
	tail_(origin.tail_),
	allocated_size_(origin.allocated_size_)
{
	content_ = std::unique_ptr<T[]>(new T[origin.allocated_size_]);
	std::copy(origin.content_.get(), origin.content_.get() + origin.allocated_size_, content_.get());
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
inline std::size_t ArrayList<T>::size() const
{
	return tail_ + 1;
}

template <typename T>
inline bool ArrayList<T>::empty() const
{
	return tail_ < 0;
}

template <typename T>
inline bool ArrayList<T>::full() const
{
	return tail_ >= static_cast<int>(allocated_size_) - 1;
}

template <typename T>
T& ArrayList<T>::operator[](int index)
{
	if (index < 0 || index > tail_)
		throw std::out_of_range("Invalid index.");
	return content_[index];
}

template <typename T>
const T& ArrayList<T>::operator[](int index) const
{
	if (index < 0 || index > tail_)
		throw std::out_of_range("Invalid index.");
	return content_[index];
}

template <typename T>
void ArrayList<T>::push_back(T element)
{
	insert(tail_+1, element);
}

template <typename T>
void ArrayList<T>::push_front(T element)
{
	insert(0, element);
}

template <typename T>
void ArrayList<T>::insert(int position, T element)
{
	if (full())
		throw std::out_of_range("Can't insert in a full list.");
	else if (position < 0 || position > tail_+1)
		throw std::out_of_range("Invalid index.");

	using std::swap;
	for (auto i = tail_ + 1; i > position; --i)
		swap(content_[i], content_[i-1]);

	tail_++;
	content_[position] = std::move(element);
}

template <typename T>
T& ArrayList<T>::back()
{
	if (empty())
		throw std::out_of_range("List is empty.");

	return content_[tail_];
}

template <typename T>
T& ArrayList<T>::front()
{
	if (empty())
		throw std::out_of_range("List is empty.");

	return content_[0];
}

template <typename T>
T ArrayList<T>::pop_back()
{
	return pop(tail_);
}

template <typename T>
T ArrayList<T>::pop_front()
{
	return pop(0);
}

template <typename T>
T ArrayList<T>::pop(int position)
{
	if (empty())
		throw std::out_of_range("Can't remove from an empty list.");
	else if (position < 0 || position > tail_)
		throw std::out_of_range("Invalid index.");

	auto target = std::move(content_[position]);

	using std::swap;
	for (auto i = position; i < tail_; ++i)
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

	int pos = i+1;
	content_[pos] = std::move(element);
	return pos;
}

template <typename T>
int ArrayList<T>::insert(T element)
{
	if (full())
		throw std::out_of_range("Can't insert in a full list.");

	return insertion(tail_++, element);
}

template <typename T>
ArrayList<T>& ArrayList<T>::operator+=(const ArrayList<T>& rhs)
{
	auto new_allocated_size = allocated_size_ + rhs.allocated_size_;
	auto new_content = std::unique_ptr<T[]>(new T[new_allocated_size]);

	for (auto i = 0; i <= tail_; ++i)
		new_content[i] = std::move(content_[i]);

	for (auto i = 0; i <= rhs.tail_; ++i) {
		T temp(rhs.content_[i]);
		new_content[i + tail_ + 1] = std::move(temp);
	}

	using std::swap;
	swap(content_, new_content);

	tail_ += rhs.size();
	allocated_size_ = new_allocated_size;

	return *this;
}

template <typename T>
ArrayList<T> operator+(ArrayList<T> lhs, const ArrayList<T>& rhs)
{
	lhs += rhs;
	return lhs;
}

template <typename T>
void ArrayList<T>::sort()
{
	// Insertion Sort
	for (auto i = 1; i <= tail_; ++i)
		insertion(i-1, content_[i]);
}

template <typename T>
int ArrayList<T>::find(const T& element, int from) const
{
	for (; from <= tail_; ++from) {
		if (content_[from] == element)
			break;
	}
	return from;
}

template <typename T>
int ArrayList<T>::remove(const T& element)
{
	auto index = find(element);
	if (index < size())
		pop(index);
	return index;
}

template <typename T>
bool ArrayList<T>::contains(const T& element) const
{
	return find(element) < size();
}

template <typename T>
unsigned ArrayList<T>::count(const T& element) const
{
	auto count = 0u;
	auto from = 0u;
	while ((from = find(element, from) + 1) <= size())
		++count;
	return count;
}

} // namespace structures

#endif // STRUCTURES_ARRAY_LIST_HPP
