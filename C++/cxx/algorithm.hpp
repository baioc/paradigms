#ifndef ALG_ALGORITHM_HPP_
#define ALG_ALGORITHM_HPP_

#include <algorithm>

namespace alg {

template <typename Iterator, typename Predicate>
auto stable_partition_position(Iterator from, Iterator to, Predicate predicate) -> Iterator
{
	const auto size = to - from;
	if (size == 0)
		return from;
	if (size == 1)
		return from + predicate(from);

	const auto mid = from + (size / 2);
	return std::rotate(
		stable_partition_position(from, mid, predicate),
		mid,
		stable_partition_position(mid, to, predicate)
	);
}

} // namespace alg

#endif // ALG_ALGORITHM_HPP_
