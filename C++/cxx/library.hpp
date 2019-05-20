#ifndef LIBRARY_HPP
#define LIBRARY_HPP

#include <vector>
#include <iostream>
#include <string>
#include <memory>
#include <cassert>


namespace lib {

template <typename T>
void draw(const T& x, std::ostream& out, std::size_t position)
{
	out << std::string(position, ' ') << x << std::endl;
}

class object {
 public:
	template <typename T>
	object(T x):
		self_(std::make_shared<model<T>>(std::move(x)))
	{}

	friend void draw(const object& x, std::ostream& out, std::size_t position)
	{
		x.self_->draw_(out, position);
	}

 private:
	struct concept {
		virtual ~concept() = default;
		virtual void draw_(std::ostream& out, std::size_t position) const = 0;
	};

	template <typename T>
	struct model final : concept {
		model(T x):
			data_(std::move(x))
		{}

		void draw_(std::ostream& out, std::size_t position) const override
		{
			draw(data_, out, position);
		}

		T data_;
	};

	std::shared_ptr<const concept> self_;
};


using document = std::vector<object>;

void draw(const document& doc, std::ostream& out, std::size_t position)
{
	out << std::string(position, ' ') << "<document>" << std::endl;
	for (const auto& elem : doc)
		draw(elem, out, position + 2);
	out << std::string(position, ' ') << "</document>" << std::endl;
}


using history = std::vector<document>;

document& current(history& h){
	assert(h.size());
	return h.back();
}

void commit(history& h)
{
	assert(h.size());
	h.push_back(current(h));
}

void undo(history& h)
{
	assert(h.size());
	h.pop_back();
}

} // namespace lib

#endif // LIBRARY_HPP
