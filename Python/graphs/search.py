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

    distance: Dict[Node, float] = {node: inf for node in graph.nodes()}
    ancestor: Dict[Node, Node] = {}
    queue: List[Node] = []

    distance[root] = 0
    ancestor[root] = None
    queue.append(root)

    while len(queue) > 0:
        u = queue.pop(0)
        for v in graph.neighbours(u):
            if v not in ancestor:  # hasn't been visited
                distance[v] = distance[u] + 1.0
                ancestor[v] = u
                queue.append(v)

    return (distance, ancestor)


def depth_first_search(graph: Graph, root: Node) -> Tuple[Dict[Node, float],
                                                          Dict[Node, Node]]:
    """Perform Depth-First Search on a Graph starting from a given vertex.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to the time taken to reach them; and whose second element contains
    nodes as keys that map to their parent node in the search tree.
    """

    time: Dict[Node, float] = {v: inf for v in graph.nodes()}
    ancestor: Dict[Node, Node] = {}
    stack: List[Node] = []

    ancestor[root] = None
    stack.append(root)

    t = -1  # next t is 0
    while len(stack) > 0:
        u = stack.pop()
        time[u] = t = t+1
        for v in [w for w in graph.neighbours(u) if w not in ancestor]:
            ancestor[v] = u
            stack.append(v)

    return (time, ancestor)


V: Set[Node] = {'1', '2', '3', '4', '5', '6', '7', '8'}
E: Set[Tuple[Node, Node]] = {('8', '3'), ('8', '4'), ('8', '5'),
                             ('3', '1'), ('3', '2'), ('4', '6'), ('5', '7')}

G: Graph = Graph(len(V))

for (u, v) in E:
    G.link(u, v)

D, T = breadth_first_search(G, '8')
print(T)

level = 0
while True:
    nodes = [v for v, d in D.items() if d == level]
    if (len(nodes) > 0):
        # nodes.sort()
        print('%d: %s' % (level, ', '.join(nodes)))
        level += 1
    else:
        break
