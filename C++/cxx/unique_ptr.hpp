#ifndef UTIL_UNIQUE_PTR_HPP
#define UTIL_UNIQUE_PTR_HPP

#include <utility>

namespace util {

template <typename T>
class unique_ptr {
 public:
	unique_ptr():
		ptr{nullptr}
	{}

	unique_ptr(T* ptr):
		ptr{ptr}
	{}

	~unique_ptr() { delete ptr; }

	T& operator*() { return *ptr; }
	const T& operator*() const { return *ptr; }

	T* operator->() { return ptr; }
	const T* operator->() const { return ptr; }

	// no copies allowed
	unique_ptr(const unique_ptr& ptr) = delete;
	unique_ptr& operator=(const unique_ptr&) = delete;

	// is actually a free function (non-member) with special access
	friend void swap(unique_ptr& lhs, unique_ptr& rhs)
	{
		using std::swap;
		swap(lhs.ptr, rhs.ptr);
	}

	// same-type move assignment via move-and-swap idiom
	unique_ptr& operator=(unique_ptr&& other)
	{
		using std::swap;
		auto aux = std::move(other);
		swap(*this, aux);
		return *this;
	}

	// give away responsibility over pointer
	T* release()
	{
		using std::swap;
		T* p = nullptr;
		swap(p, ptr);
		return p;
	}

	// needed in order to move between unique_ptrs of different types
	template <typename Other>
	friend class unque_ptr;

	template <typename Other>
	unique_ptr(unique_ptr<Other>&& other):
		ptr{other.release()}
	{}

	// reconstructs unique_ptr with given argument
	void reset(T* p = nullptr)
	{
		auto aux = unique_ptr{p};
		swap(*this, aux);
	}

	// works even if moving between different (although compatible) types
	template <typename Other>
	unique_ptr& operator=(unique_ptr&& other)
	{
		reset(other.release());
		return *this;
	}

 private:
	T* ptr;
};

// variadic template arguments
template <typename T, typename ...Args>
unique_ptr<T> make_unique(Args&& ...args)
{
	return unique_ptr<T>{ new T{std::forward<Args>(args)...} };
}

} // namespace util

#endif // UTIL_UNIQUE_PTR_HPP
