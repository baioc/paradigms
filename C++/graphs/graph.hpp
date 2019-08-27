/*
 * Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
 *
 * @License Apache <https://gitlab.com/baioc/paradigms>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef STRUCTURES_GRAPH_HPP
#define STRUCTURES_GRAPH_HPP

#include <unordered_map>
#include <utility> // move, pair
#include <limits> // infinity


namespace structures {

using std::unordered_map;


template <typename Label, typename Weight, bool directed=false>
	// requires Hashable<Label>,
	//          LessThanComparable<Weight>,
	//          Assignable<Weight,1>,
	//          std::numeric_limits<Weight>::has_infinity(),
class Graph {
 public:
	Graph() = default;
	explicit Graph(int);

	int node_number() const; // O(1)
	int edge_number() const; // O(1)

	bool insert(Label); // O(1)
	int erase(const Label&); // O(V)

	int link(const Label&, const Label&, Weight=1); // O(1)
	int unlink(const Label&, const Label&); // O(1)

	bool contains(const Label&) const; // O(1)
	int degree(const Label&) const; // directed ? O(V) : O(1)
	int degree_out(const Label&) const; // O(1)
	int degree_in(const Label&) const; // directed ? O(V) : O(1)

	bool contains(const Label&, const Label&) const; // O(1)
	Weight weight(const Label&, const Label&) const; // O(1)

	// returning vectors or using output iterators would be cleaner; these
	// methods' signatures are such only in order to get O(1) time complexity
	const unordered_map<Label,unordered_map<Label,Weight>>& nodes() const;
	const unordered_map<Label,Weight>& neighbours(const Label&) const;

 private:
	unordered_map<Label,unordered_map<Label,Weight>> adjacencies_;
	int edges_{0};
	static constexpr Weight infinity_{std::numeric_limits<Weight>::infinity()};
};


template <typename L, typename W, bool d>
Graph<L,W,d>::Graph(int initial_node_capacity)
{
	adjacencies_.reserve(initial_node_capacity);
}

template <typename L, typename W, bool d>
inline int Graph<L,W,d>::node_number() const
{
	return adjacencies_.size();
}

template <typename L, typename W, bool d>
inline int Graph<L,W,d>::edge_number() const
{
	return edges_;
}

template <typename L, typename W, bool d>
inline bool Graph<L,W,d>::insert(L node)
{
	unordered_map<L,W> empty = {};
	const auto ret = adjacencies_.emplace(std::move(node), std::move(empty));
	return ret.second; // map's signaling if emplace occurred
}

template <typename L, typename W, bool d>
int Graph<L,W,d>::erase(const L& node)
{
	if (!contains(node))
		return 0;

	int erased = adjacencies_[node].size();
	adjacencies_.erase(node);

	for (auto& assoc: adjacencies_)
		erased += assoc.second.erase(node);

	return erased; // number of erased edges
}

template <typename L, typename W, bool directed>
int Graph<L,W,directed>::link(const L& node_from, const L& node_to, W weight)
{
	// remove "unlinks", see criteria for contains(link) method
	if (!(weight < infinity_))
		return -1 * unlink(node_from, node_to); // -no. of removed links
	else if (node_from == node_to)
		return 0; // ignore reflexive edges

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
int Graph<L,W,directed>::unlink(const L& node_from, const L& node_to)
{
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
inline bool Graph<L,W,d>::contains(const L& node) const
{
	return adjacencies_.find(node) != adjacencies_.end();
}

template <typename L, typename W, bool directed>
inline int Graph<L,W,directed>::degree(const L& node) const
{
	if constexpr (!directed)
		return degree_out(node);
	else
		return contains(node) ? degree_out(node) + degree_in(node) : -1;
}

template <typename L, typename W, bool directed>
inline int Graph<L,W,directed>::degree_out(const L& node) const
{
	return contains(node) ? adjacencies_.at(node).size() : -1;
}

template <typename L, typename W, bool directed>
int Graph<L,W,directed>::degree_in(const L& node) const
{
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
inline bool Graph<L,W,d>::contains(const L& node_from, const L& node_to) const
{
	return weight(node_from, node_to) < infinity_;
}

template <typename L, typename W, bool d>
W Graph<L,W,d>::weight(const L& node_from, const L& node_to) const
{
	if (contains(node_from)) {
		const auto& adj = adjacencies_.at(node_from);
		const auto& pos = adj.find(node_to);
		if (pos != adj.end())
			return pos->second;
	}
	return infinity_;
}

template <typename L, typename W, bool d>
const unordered_map<L,unordered_map<L,W>>& Graph<L,W,d>::nodes() const
{
	return adjacencies_;
}

template <typename L, typename W, bool d>
const unordered_map<L,W>& Graph<L,W,d>::neighbours(const L& node) const
{
	unordered_map<L,W> empty = {};
	return contains(node) ? adjacencies_.at(node) : empty;
}

} // namespace structures

#endif // STRUCTURES_GRAPH_HPP
