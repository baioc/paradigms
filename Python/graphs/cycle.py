# Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
# @License Apache <https://gitlab.com/baioc/paradigms>

from graphs import Digraph, Graph
from typing import Set, Tuple, List, Sequence, TypeVar, Generator, Dict, \
                   FrozenSet
from math import inf
from itertools import combinations

Node = str
T = TypeVar('T')  # generic Type


def eulerian_cycle(graph: Digraph, begin: Node = None) -> List[Node]:
    """Finds an Eulerian cycle on a Digraph using Hierholzer's algorithm.

    Returns either a list representing the node path order of the Eulerian
    cycle; or None when no such cycle is found.
    """

    def arbitrary(seq: Sequence[T]) -> T:
        for x in seq:
            return x

    def graph_edges(g: Digraph) -> Generator[Tuple[Node, Node], None, None]:
        for u in g.nodes():
            for v in g.neighbours(u):
                yield (u, v)

    def Hierholzer(graph: Digraph,
                   begin: Node,
                   traversed: Set[Tuple[Node, Node]]) -> List[Node]:
        def splicycle(cycle: List[T], subcycle: List[T]) -> List[T]:
            pos = cycle.index(subcycle[0])
            return cycle[:pos] + subcycle + cycle[pos+1:]

        cycle = [begin]
        u = begin
        while True:
            e = None
            for v in graph.neighbours(u):
                if (u, v) not in traversed:
                    e = (u, v)
                    break
            else:  # no break: every edge (u,v) has already been traversed
                return None

            (_, v) = e
            traversed[e] = True
            # traversed[(v, u)] = True  # @NOTE: only for undirected graphs
            cycle.append(v)
            u = v
            if u == begin:
                break

        for v in cycle:
            for w in graph.neighbours(v):
                if (v, w) not in traversed:
                    subcycle = Hierholzer(graph, v, traversed)
                    if subcycle is None:
                        return None
                    else:
                        cycle = splicycle(cycle, subcycle)

        return cycle

    traversed = {}
    begin = arbitrary(graph.nodes()) if begin is None else begin
    cycle = Hierholzer(graph, begin, traversed)
    if cycle is None:
        return None
    else:
        for (u, v) in graph_edges(graph):
            if (u, v) not in traversed:
                return None
        else:  # no break
            return cycle


def hamiltonian_circuit(graph: Graph, start: Node) -> float:
    """Finds a Graph's minimal Hamiltonian circuit through Bellman-Held-Karp.

    Supposes the Graph is connected and has at least one Hamiltonian cycle.
    Returns the total cost of the optimal circuit path; or infinity when none
    is found.
    """

    Visits, FinalDestination, Cost = FrozenSet[Node], Node, float
    cost: Dict[Tuple[Visits, FinalDestination], Cost] = {}
    dests = dict.fromkeys(graph.nodes())
    dests.pop(start)
    dests = frozenset(dests)

    for place in dests:
        cost[(frozenset({place}), place)] = graph.weight(start, place)

    for size in range(2, graph.node_number()):
        for itinerary in combinations(dests, size):
            route = frozenset(itinerary)
            for final in route:
                sub = route - {final}
                opt = inf  # optimal solution for problem subset
                for mid in sub:
                    opt = min(opt, cost[(sub, mid)] + graph.weight(mid, final))
                cost[(route, final)] = opt

    minimum = inf
    for end in dests:
        minimum = min(minimum, cost[(dests, end)] + graph.weight(end, start))
    return minimum


V: Set[Node] = {'a', 'b', 'c', 'd', 'e'}
E: Set[Tuple[Node, Node, float]] = {('b', 'a', 2.5),  # ('a', 'c', 3),
                                    ('c', 'd', 2.5),  # ('d', 'b', 1),
                                    ('a', 'e', 4), ('e', 'c', 2),
                                    ('e', 'b', 1.5), ('d', 'e', 2)}

G: Digraph = Digraph(len(V))

for (u, v, w) in E:
    G.link(u, v, w)

C = eulerian_cycle(G, 'a')
print(C)
