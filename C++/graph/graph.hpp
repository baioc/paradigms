#ifndef STRUCTURES_GRAPH_HPP
#define STRUCTURES_GRAPH_HPP

#include <unordered_map>
#include <utility> // move, pair
#include <limits> // infinity
#include <cassert> // assert
#include <vector>
#include <iterator> // back_inserter


namespace structures {

template <typename Label, typename Weight, bool directed=false>
	// requires Hashable<Label>
	//          => CopyConstructible<Label>
	//             => MoveConstructible<Label>
	// requires std::numeric_limits<Weight>::has_infinity(),
	//          LessThanComparable<Weight>,
	//          Assignable<Weight,1>,
	//          CopyConstructible<Weight>,
	//          MoveAssignable<Weight>
class Graph {
 public:
	Graph() = default;
	Graph(int initial_vertice_number);

	int vertice_number() const;
	int edge_number() const;

	bool insert(Label);
	int erase(const Label&); // O(V)

	int link(const Label&, const Label&, Weight=1);
	int unlink(const Label&, const Label&);

	bool contains(const Label&) const;
	int degree(const Label&) const; // directed ? O(V) : O(1)
	int degree_out(const Label&) const; // O(1)
	int degree_in(const Label&) const; // directed ? O(V) : O(1)

	bool contains(const Label&, const Label&) const;
	Weight weight(const Label&, const Label&) const;

	template <typename OutputIterator>
	void vertices(OutputIterator) const; // O(V)

	template <typename OutputIterator>
	void neighbours(const Label&, OutputIterator) const; // O(E(v))

	template <typename OutputIterator>
	void edges(const Label&, OutputIterator) const; // O(E(v))

	std::vector<Label> vertices() const; // O(V)
	std::vector<Label> neighbours(const Label&) const; // O(E(v))
	std::unordered_map<Label,Weight> edges(const Label&) const; // O(E(v))

 private:
	std::unordered_map<Label,std::unordered_map<Label,Weight>> adjacencies_;
	static constexpr Weight infinity_{std::numeric_limits<Weight>::infinity()};
	int edges_{0};
};


template <typename L, typename W, bool d>
Graph<L,W,d>::Graph(int initial_vertice_number) {
	assert(initial_vertice_number >= 0);
	adjacencies_.reserve(initial_vertice_number);
}

template <typename L, typename W, bool d>
inline int Graph<L,W,d>::vertice_number() const {
	return adjacencies_.size();
}

template <typename L, typename W, bool d>
inline int Graph<L,W,d>::edge_number() const {
	return edges_;
}

template <typename L, typename W, bool d>
inline bool Graph<L,W,d>::insert(L node) {
	std::unordered_map<L,W> empty{};
	const auto ret = adjacencies_.emplace(std::move(node), std::move(empty));
	return ret.second; // map's signaling if emplace occurred
}

template <typename L, typename W, bool d>
int Graph<L,W,d>::erase(const L& node) {
	if (!contains(node))
		return 0;

	int erased = adjacencies_[node].size();
	adjacencies_.erase(node);

	for (auto& assoc: adjacencies_)
		erased += assoc.second.erase(node);

	return erased; // number of erased edges
}

template <typename L, typename W, bool directed>
int Graph<L,W,directed>::link(const L& node_from, const L& node_to, W weight) {
	// remove "unlinks", see criteria for contains(link) method
	if (!(weight < infinity_))
		return -1 * unlink(node_from, node_to); // -number of removed links

	// inserts any unregistered nodes before linking
	const int inserted = insert(node_from) + insert(node_to);

	// either made a new link
	if (!contains(node_from, node_to))
		++edges_;

	// or simply updated its weight
	adjacencies_[node_from][node_to] = std::move(weight);
	if constexpr (!directed)
		adjacencies_[node_to][node_from] = std::move(weight);

	return inserted; // number of implicitly created nodes
}

template <typename L, typename W, bool directed>
int Graph<L,W,directed>::unlink(const L& node_from, const L& node_to) {
	int disconnected = 0;

	if (contains(node_from) && contains(node_to)) {
		disconnected += adjacencies_[node_from].erase(node_to);

		if (disconnected)
			--edges_;

		if constexpr (!directed)
			disconnected += adjacencies_[node_to].erase(node_from);
	}

	return disconnected; // number of removed links
}

template <typename L, typename W, bool d>
inline bool Graph<L,W,d>::contains(const L& node) const {
	return adjacencies_.find(node) != adjacencies_.end();
}

template <typename L, typename W, bool directed>
inline int Graph<L,W,directed>::degree(const L& node) const {
	if constexpr (!directed)
		return degree_out(node);
	else
		return contains(node) ? degree_out(node) + degree_in(node) : -1;
}

template <typename L, typename W, bool directed>
inline int Graph<L,W,directed>::degree_out(const L& node) const {
	return contains(node) ? adjacencies_.at(node).size() : -1;
}

template <typename L, typename W, bool directed>
int Graph<L,W,directed>::degree_in(const L& node) const {
	if constexpr (!directed) {
		return degree_out(node);

	} else {
		if (!contains(node))
			return -1;

		int sum = 0;
		for (const auto& assoc: adjacencies_) {
			const auto& adj = assoc.second;
			sum += adj.count(node);
		}

		return sum;
	}
}

template <typename L, typename W, bool d>
inline bool Graph<L,W,d>::contains(const L& node_from, const L& node_to) const {
	return weight(node_from, node_to) < infinity_;
}

template <typename L, typename W, bool d>
W Graph<L,W,d>::weight(const L& node_from, const L& node_to) const {
	if (contains(node_from)) {
		const auto& adj = adjacencies_.at(node_from);
		const auto& pos = adj.find(node_to);
		if (pos != adj.end())
			return pos->second;
	}
	return infinity_;
}

template <typename L, typename W, bool d>
template <typename OutIter>
void Graph<L,W,d>::vertices(OutIter out) const {
	for (const auto& assoc: adjacencies_)
		*out++ = assoc.first;
}

template <typename L, typename W, bool d>
template <typename OutIter>
void Graph<L,W,d>::neighbours(const L& node, OutIter out) const {
	if (!contains(node)) return;
	for (const auto& adjacency: adjacencies_.at(node))
		*out++ = adjacency.first;
}

template <typename L, typename W, bool d>
template <typename OutIter>
void Graph<L,W,d>::edges(const L& node, OutIter out) const {
	if (!contains(node)) return;
	for (auto& edge: adjacencies_.at(node))
		*out++ = edge;
}

template <typename L, typename W, bool d>
inline std::vector<L> Graph<L,W,d>::vertices() const {
	std::vector<L> result{};
	vertices(std::back_inserter(result));
	return result;
}

template <typename L, typename W, bool d>
inline std::vector<L> Graph<L,W,d>::neighbours(const L& node) const {
	std::vector<L> result{};
	neighbours(node, std::back_inserter(result));
	return result;
}

template <typename L, typename W, bool d>
inline std::unordered_map<L,W> Graph<L,W,d>::edges(const L& node) const {
	std::unordered_map<L,W> empty{};
	return contains(node) ? adjacencies_.at(node) : empty;
}

} // namespace structures

#endif // STRUCTURES_GRAPH_HPP
