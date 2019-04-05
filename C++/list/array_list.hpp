#ifndef STRUCTURES_ARRAY_LIST_HPP
#define STRUCTURES_ARRAY_LIST_HPP

#include <cstdint>	// std::size_t

namespace structures {

template <typename T>
	// requires MoveAssignable<T>
	// && Sortable<T>
	// && Equality_comparable<T>
class ArrayList {
public:
	explicit ArrayList(int);
	ArrayList(): ArrayList(DEFAULT_SIZE_) {}
	ArrayList(initializer_list<T>);

	ArrayList(const ArrayList&);
	ArrayList& operator=(const ArrayList&);
	ArrayList(ArrayList&&);
	ArrayList& operator=(ArrayList&&);

	T& operator[](int);
	const T& operator[](int) const;
	ArrayList& operator+=(const ArrayList&);

	void insert(int, T);
	int insert(T);
	void remove(int);
	T pop(int);

	bool empty() const;
	std::size_t size() const;

	void sort();
	void merge(const ArrayList&);

	int remove(const T&);
	int find(const T&, int=0) const;
    bool contains(const T& data) const;
	int count(const T&) const;

private:
	static const auto DEFAULT_SIZE_ = 16u;

	std::unique_ptr<T[]> content_;
	std::size_t current_size_{0};
	std::size_t allocated_size_;

	bool full() const;

	friend void swap(ArrayList<T>& a, ArrayList<T>& b)
	{
		using std::swap;
		swap(a.content_, b.content_);
		swap(a.current_size_, b.current_size_);
		swap(a.allocated_size_, b.allocated_size_);
	}
};

template <typename T>
ArrayList<T> operator+(const ArrayList<T>&, const ArrayList<T>&);

} // namespace structures

#endif // STRUCTURES_ARRAY_LIST_HPP
