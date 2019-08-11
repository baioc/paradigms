#ifndef STRUCTURES_GRAPH_HPP
#define STRUCTURES_GRAPH_HPP

#include <vector>
#include <unordered_map>
#include <utility> // pair, move
#include <limits> // infinity
#include <algorithm> // remove_if, find_if
#include <iterator> // back_inserter


namespace structures {

template <typename Label>
class Graph {
 public:
	Graph() = default;

	Graph(int vertice_number) {
		adjacencies_.reserve(vertice_number);
	}


	bool insert(Label node) {
		std::vector<std::pair<Label,double>> empty{};
		return adjacencies_.emplace(std::move(node), std::move(empty)).second;
	}

	bool erase(const Label& node) {
		if (adjacencies_.erase(node) == 0)
			return false;

		for (auto& assoc : adjacencies_) {
			auto& adj = assoc.second;
			adj.erase(
				std::remove_if(
					adj.begin(), adj.end(),
					[node](auto p){return p.first == node;}
				),
				adj.end()
			);
		}

		return true;
	}


	bool link(const Label& node_from, const Label& node_to, double weight=1.0) { // @weighted
		// @NOTE: inserts any unregistered nodes before linking
		const auto f = insert(node_from);
		const auto t = insert(node_to);

		adjacencies_[node_from].emplace_back(node_to, weight);
		++edges_;

		adjacencies_[node_to].emplace_back(node_from, weight); // @undirected

		return !f && !t;
	}

	bool unlink(const Label& node_from, const Label& node_to, double weight=1.0) { // @weighted
		if (!contains(node_from) || !contains(node_to))
			return false;

		auto& from_adj = adjacencies_[node_from];
		const auto to_pos = std::find_if(
			from_adj.begin(), from_adj.end(),
			[node_to, weight](auto p){
				return p.first == node_to && p.second == weight;
			}
		);
		if (to_pos != from_adj.end()) {
			from_adj.erase(to_pos);
			--edges_;
		}

		// @undirected
		auto& to_adj = adjacencies_[node_to];
		const auto from_pos = std::find_if(
			to_adj.begin(), to_adj.end(),
			[node_from, weight](auto p) {
				return p.first == node_from && p.second == weight;
			}
		);
		if (from_pos != to_adj.end())
			to_adj.erase(from_pos);

		return true;
	}


	int vertice_number() const {
		return adjacencies_.size();
	}

	int edge_number() const {
		return edges_;
	}

	template <typename OutputIterator>
	void vertices(OutputIterator out) const {
		for (const auto& assoc : adjacencies_)
			*out++ = assoc.first;
	}

	std::vector<Label> vertices() const {
		std::vector<Label> result{};
		vertices(std::back_inserter(result));
		return result;
	}


	int degree(const Label& node) const { // @undirected
		if (contains(node))
			return adjacencies_.at(node).size();
		else
			return -1;
	}


	template <typename OutputIterator>
	void neighbours(const Label& node, OutputIterator out) const {
		if (!contains(node)) return;
		for (const auto& p : adjacencies_.at(node))
			*out++ = p.first;
	}

	std::vector<Label> neighbours(const Label& node) const {
		std::vector<Label> result{};
		neighbours(node, std::back_inserter(result));
		return result;
	}


	bool contains(const Label& node) const {
		return adjacencies_.find(node) != adjacencies_.end();
	}

	double weight(const Label& node_from, const Label& node_to) const {
		if (contains(node_from)) {
			for (const auto& p : adjacencies_.at(node_from)) {
				if (p.first == node_to)
					return p.second;
			}
		}
		return std::numeric_limits<double>::infinity();
	}

	bool contains(const Label& node_from, const Label& node_to) const {
		return weight(node_from, node_to) < std::numeric_limits<double>::infinity();
	}

 private:
	std::unordered_map<Label,std::vector<std::pair<Label,double>>> adjacencies_;
	int edges_{0};
};

} // namespace structures

#endif // STRUCTURES_GRAPH_HPP
