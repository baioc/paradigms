# Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
# @License Apache <https://gitlab.com/baioc/paradigms>

from .graphs import Digraph, Graph
from typing import Set, Tuple, Dict, Optional, Generator, Union
from math import inf
from pprint import pprint

Node = str


def shortest_route(graph: Union[Graph, Digraph], start: Node) \
        -> Tuple[Dict[Node, float], Dict[Node, Optional[Node]]]:
    """
    Compute shortest paths from a single vertex to all others in a graph using
    the Bellman-Ford algorithm.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to their distance from the start; and whose second element contains
    nodes as keys that map to their antecessor in the shortest-path tree.

    Raises a ValueError exception in case a negative cycle is found.
    """

    def graph_edges(g: Union[Graph, Digraph]) \
            -> Generator[Tuple[Node, Node], None, None]:
        for u in g.nodes():
            for v in g.neighbours(u):
                yield (u, v)

    # initialize
    distances: Dict[Node, float] = dict.fromkeys(graph.nodes(), inf)
    antecessors: Dict[Node, Optional[Node]] = {}
    distances[start] = 0

    for _ in range(1, graph.node_number()):
        done = True
        for (u, v) in graph_edges(graph):
            # relax
            Duv = distances[u] + graph.weight(u, v)
            if Duv < distances[v]:
                distances[v] = Duv
                antecessors[v] = u
                done = False
        if done:
            break

    # report negative cycle
    for (u, v) in graph_edges(graph):
        if distances[u] + graph.weight(u, v) < distances[v]:
            raise ValueError("Negative cycle detected")

    # disconnected vertices
    for v in distances:
        if v not in antecessors:
            antecessors[v] = None

    return (distances, antecessors)


def shortest_path(graph: Union[Graph, Digraph], source: Node) \
        -> Tuple[Dict[Node, float], Dict[Node, Optional[Node]]]:
    """
    Use Dijkstra's Shortest Path First algorithm to find the shortest path
    between a given origin and all other nodes in a graph.

    Does not guarantee a shortest path when presented with negative weights.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to their distance from the source; and whose second element contains
    nodes as keys that map to their antecessor in the shortest-path tree.
    """

    # initialize
    distances: Dict[Node, float] = {}
    antecessors: Dict[Node, Optional[Node]] = {}
    unclosed: Set[Node] = set()  # TODO: an updateable min-heap would be better
    for v in graph.nodes():
        distances[v] = inf if v != source else 0
        antecessors[v] = None
        unclosed.add(v)

    while len(unclosed) > 0:
        u = min(unclosed, key=distances.get)
        unclosed.remove(u)
        for v in graph.neighbours(u):
            if v in unclosed:
                # relax
                Duv = distances[u] + graph.weight(u, v)
                if Duv < distances[v]:
                    distances[v] = Duv
                    antecessors[v] = u

    return (distances, antecessors)


def shortest_network(graph: Union[Graph, Digraph]) \
        -> Dict[Node, Dict[Node, float]]:
    """Find shortest paths for all vertex pairs in a graph via Floyd–Warshall.

    Returns a bidimensional dictionary D that uses node labels as indexes such
    that D[u][v] is the shortest circuit cost going from u to v.
    """

    vertices = graph.nodes()
    dist = {u: {v: graph.weight(u, v) for v in vertices} for u in vertices}

    # for every vertex, check if it is a shortcut between two pairs
    for interm in vertices:
        for source in vertices:
            for destination in vertices:
                shc = dist[source][interm] + dist[interm][destination]
                dist[source][destination] = min(dist[source][destination], shc)

    return dist


def _path_test():
    V: Set[Node] = {'A', 'B', 'C', 'S'}
    E: Set[Tuple[Node, Node, float]] = {('S', 'A', 5), ('S', 'B', 3),
                                        ('B', 'A', 1),
                                        ('A', 'C', 6), ('B', 'C', 4)}
    G: Union[Graph, Digraph] = Graph(len(V))
    for (u, v, w) in E:
        G.link(u, v, w)

    (D, T) = shortest_path(G, 'S')
    for v in V:
        path = []
        tail = v
        while tail is not None:
            path.insert(0, tail)
            u = T[tail]
            tail = u
        print('<%s> = %g' % (', '.join(path), D[v]))

    N = shortest_network(G)
    pprint(N)
