#ifndef UTIL_UNIQUE_PTR_HPP
#define UTIL_UNIQUE_PTR_HPP

#include <utility>

namespace util {

template <typename T>
class unique_ptr {
 public:
	unique_ptr(): ptr_{nullptr} {}
	unique_ptr(T* ptr): ptr_{ptr} {}

	~unique_ptr() { delete ptr_; }

	T& operator*() { return *ptr_; }
	const T& operator*() const { return *ptr_; }

	T* operator->() { return ptr_; }
	const T* operator->() const { return ptr_; }

	// no copies allowed
	unique_ptr(const unique_ptr& ptr) = delete;
	unique_ptr& operator=(const unique_ptr&) = delete;

	// is actually a free function (non-member) with special access
	friend void swap(unique_ptr& lhs, unique_ptr& rhs)
	{
		using std::swap;
		swap(lhs.ptr_, rhs.ptr_);
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
		swap(p, ptr_);
		return p;
	}

	// needed in order to move between unique_ptrs of different types
	template <typename Other>
	friend class unque_ptr;

	template <typename Other>
	unique_ptr(unique_ptr<Other>&& other):
		ptr_{other.release()}
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

 protected:
	T* ptr_;
};

// variadic template arguments
template <typename T, typename ...Args>
unique_ptr<T> make_unique(Args&& ...args)
{
	return unique_ptr<T>{ new T{std::forward<Args>(args)...} };
}

} // namespace util

#endif // UTIL_UNIQUE_PTR_HPP
