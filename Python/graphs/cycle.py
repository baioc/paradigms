# Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
# @License Apache <https://gitlab.com/baioc/paradigms>

from graphs import Graph
from typing import Set, Tuple, List, Sequence, TypeVar, Generator, Dict, \
                   FrozenSet
from itertools import combinations

Node = str
T = TypeVar('T')  # generic Type


def eulerian_cycle(graph: Graph,
                   begin: Node = None,
                   traversed: Set[Tuple[Node, Node]] = None) -> List[Node]:
    """Finds a Graph's Eulerian cycle, if present, with Hierholzer's algorithm.

    Returns either a list representing the node path order of the Eulerian
    cycle; or None when no such cycle is found.
    """

    def arbitrary(seq: Sequence[T]) -> T:
        for x in seq:
            return x

    def graph_edges(graph: Graph) -> Generator[Tuple[Node, Node], None, None]:
        for u in graph.nodes():
            for v in graph.neighbours(u):
                yield (u, v)

    def Hierholzer(graph: Graph,
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

            (u, v) = e
            traversed[(u, v)] = True
            traversed[(v, u)] = True  # undirected
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

    traversed = {} if traversed is None else traversed
    begin = arbitrary(graph.nodes()) if begin is None else begin
    cycle = Hierholzer(graph, begin, traversed)
    if cycle is None:
        return None
    else:
        for (a, b) in graph_edges(graph):
            if (a, b) not in traversed:
                return None
        else:  # no break
            return cycle


def hamiltonian_circuit(graph: Graph, start: Node) -> float:
    """Finds a Graph's minimal Hamiltonian cycle through Bellman-Held-Karp.

    Supposes the Graph is connected and has at least one Hamiltonian cycle.
    Returns the total cost of the optimal circuit path.
    """

    Visits, Last, Cost = FrozenSet[Node], Node, float
    cost: Dict[Tuple[Visits, Last], Cost] = {}
    dests = frozenset({v for v in graph.nodes() if v != start})

    for place in dests:
        cost[(frozenset({place}), place)] = graph.weight(start, place)

    for size in range(2, graph.node_number()):
        for itinerary in combinations(dests, size):
            route = frozenset(itinerary)
            for final in route:
                sub = route - {final}
                paths = {cost[(sub, m)] + graph.weight(m, final) for m in sub}
                cost[(route, final)] = min(paths)

    dist_sum = {cost[(dests, end)] + graph.weight(end, start) for end in dests}
    return min(dist_sum)


V: Set[Node] = {'a', 'b', 'c', 'd', 'e'}
E: Set[Tuple[Node, Node, float]] = {('a', 'e', 4), ('a', 'c', 3),
                                    ('d', 'b', 1),  # ('d', 'e', 1.5),
                                    ('c', 'd', 2.5),  # ('b', 'a', 2.5),
                                    ('e', 'b', 1.5)}  # , ('e', 'c', 1)}

G: Graph = Graph(len(V))

for (u, v, w) in E:
    G.link(u, v, w)

C = eulerian_cycle(G, 'e')
print(C)
