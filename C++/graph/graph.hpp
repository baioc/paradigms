#ifndef STRUCTURES_GRAPH_HPP
#define STRUCTURES_GRAPH_HPP

#include <unordered_map>
#include <utility> // move, pair
#include <limits> // infinity
#include <vector>
#include <iterator> // back_inserter
#include <cassert>


namespace structures {

template <typename Label>
class Graph {
 public:
	using Weight = double;

	Graph() = default;

	Graph(int initial_vertice_number) {
		assert(initial_vertice_number >= 0);
		adjacencies_.reserve(initial_vertice_number);
	}


	bool insert(Label node) {
		std::unordered_map<Label,Weight> empty{};
		const auto ret = adjacencies_.emplace(std::move(node), std::move(empty));
		return ret.second; // map's signaling if emplace occurred
	}

	int erase(const Label& node) {
		if (!contains(node)) return 0;

		int erased = adjacencies_[node].size();
		adjacencies_.erase(node);

		for (auto& assoc: adjacencies_)
			erased += assoc.second.erase(node);

		return erased; // number of erased edges
	}


	int link(const Label& node_from, const Label& node_to, Weight weight=weight_unit) {
		// remove "unlinks", see criteria for contains(link) method
		if (weight >= weight_inf)
			return -1 * unlink(node_from, node_to); // -number of removed links

		// inserts any unregistered nodes before linking
		const int inserted = insert(node_from) + insert(node_to);
		if (!contains(node_from, node_to)) ++edges_;
		adjacencies_[node_from][node_to] = weight;
		adjacencies_[node_to][node_from] = weight; // @undirected

		return inserted; // number of implicitly created nodes
	}

	int unlink(const Label& node_from, const Label& node_to) {
		int disconnected = 0;
		if (contains(node_from) && contains(node_to)) {
			disconnected += adjacencies_[node_from].erase(node_to);
			if (disconnected) --edges_;
			disconnected += adjacencies_[node_to].erase(node_from); // @undirected
		}
		return disconnected; // number of removed links
	}


	int vertice_number() const {
		return adjacencies_.size();
	}

	int edge_number() const {
		return edges_;
	}

	template <typename OutputIterator>
	void vertices(OutputIterator out) const {
		for (const auto& assoc: adjacencies_)
			*out++ = assoc.first;
	}

	std::vector<Label> vertices() const {
		std::vector<Label> result{};
		vertices(std::back_inserter(result));
		return result;
	}


	bool contains(const Label& node) const {
		return adjacencies_.find(node) != adjacencies_.end();
	}

	int degree(const Label& node) const { // @undirected
		return contains(node) ? adjacencies_.at(node).size() : -1;
	}

	template <typename OutputIterator>
	void neighbours(const Label& node, OutputIterator out) const {
		if (!contains(node)) return;
		for (const auto& adjacency: adjacencies_.at(node))
			*out++ = adjacency.first;
	}

	std::vector<Label> neighbours(const Label& node) const {
		std::vector<Label> result{};
		neighbours(node, std::back_inserter(result));
		return result;
	}

	template <typename OutputIterator>
	void edges(const Label& node, OutputIterator out) const {
		if (!contains(node)) return;
		for (const auto& edge: adjacencies_.at(node))
			*out++ = edge;
	}

	std::unordered_map<Label,Weight> edges(const Label& node) const {
		std::unordered_map<Label,Weight> empty{};
		return contains(node) ? adjacencies_.at(node) : empty;
	}


	bool contains(const Label& node_from, const Label& node_to) const {
		return weight(node_from, node_to) < weight_inf;
	}

	Weight weight(const Label& node_from, const Label& node_to) const {
		if (contains(node_from)) {
			const auto& adj = adjacencies_.at(node_from);
			const auto& pos = adj.find(node_to);
			if (pos != adj.end())
				return pos->second;
		}
		return weight_inf;
	}

 private:
	std::unordered_map<Label,std::unordered_map<Label,Weight>> adjacencies_;
	int edges_{0};
	static constexpr Weight weight_unit = 1;
	static constexpr Weight weight_inf{std::numeric_limits<Weight>::infinity()};
};

} // namespace structures

#endif // STRUCTURES_GRAPH_HPP
