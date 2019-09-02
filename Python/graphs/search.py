# Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
# @License Apache <https://gitlab.com/baioc/paradigms>

from graphs import Graph
from math import inf
from typing import Set, Tuple, Dict, List

Node = str


def breadth_first_search(graph: Graph, root: Node) -> Tuple[Dict[Node, float],
                                                            Dict[Node, Node]]:
    """Perform Breadth-First Search on a Graph starting from a given vertex.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to their distance from the starting one, in edge units; and whose
    second element contains nodes as keys that map to their parent node in the
    search tree.
    """

    distances: Dict[Node, float] = dict.fromkeys(graph.nodes(), inf)
    ancestors: Dict[Node, Node] = {}
    queue: List[Node] = []

    distances[root] = 0
    ancestors[root] = None
    queue.append(root)

    while len(queue) > 0:
        u = queue.pop(0)
        for v in graph.neighbours(u):
            if v not in ancestors:  # hasn't been visited
                distances[v] = distances[u] + 1
                ancestors[v] = u
                queue.append(v)

    for v in distances:
        if v not in ancestors:
            ancestors[v] = None

    return (distances, ancestors)


def depth_first_search(graph: Graph, root: Node) -> Tuple[Dict[Node, float],
                                                          Dict[Node, Node]]:
    """Perform Depth-First Search on a Graph starting from a given vertex.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to the time taken to reach them; and whose second element contains
    nodes as keys that map to their parent node in the search tree.
    """

    times: Dict[Node, float] = dict.fromkeys(graph.nodes(), inf)
    ancestors: Dict[Node, Node] = {}
    stack: List[Node] = []

    ancestors[root] = None
    stack.append(root)

    t = -1  # next t is 0
    while len(stack) > 0:
        u = stack.pop()
        times[u] = t = t + 1
        for v in graph.neighbours(u):
            if v not in ancestors:  # hasn't been visited
                ancestors[v] = u
                stack.append(v)

    for v in times:
        if v not in ancestors:
            ancestors[v] = None

    return (times, ancestors)


V: Set[Node] = {'1', '2', '3', '4', '5', '6', '7', '8'}
E: Set[Tuple[Node, Node]] = {('8', '3'), ('8', '4'), ('8', '5'), ('8', '1'),
                             ('3', '1'), ('3', '2'), ('4', '6'), ('5', '7')}

G: Graph = Graph(len(V))

for (u, v) in E:
    G.link(u, v)

D, T = breadth_first_search(G, '8')
print(D)
print(T)

level = 0
while True:
    nodes = [v for v, d in D.items() if d == level]
    if (len(nodes) > 0):
        nodes.sort()
        print('%d: %s' % (level, ', '.join(nodes)))
        level += 1
    else:
        break
