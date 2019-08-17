from graphs import Graph
from math import inf
from typing import Dict, Tuple, List


def bfs(graph: Graph, start: str) -> Tuple[Dict[str, float], Dict[str, str]]:
    """Perform Breadth-First Search on a Graph starting from a given vertex.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to their distance from the starting one; and whose second element
    contains nodes as keys that map to their parent node in the search tree.
    """

    distance: Dict[str, float] = {v: inf for v in graph.vertices()}
    parent: Dict[str, str] = {}

    distance[start] = 0
    parent[start] = None

    queue: List[str] = []
    queue.append(start)

    while len(queue) > 0:
        current = queue.pop(0)

        for next in graph.neighbours(current):
            if next not in parent:
                distance[next] = distance[current] + 1.0
                parent[next] = current

                queue.append(next)

    return (distance, parent)


def dfs(graph: Graph, start: str) -> Tuple[Dict[str, float], Dict[str, str]]:
    """Perform Depth-First Search on a Graph starting from a given vertex.

    Returns a dictionary tuple whose first element contains nodes as keys that
    map to the time taken to reach them; and whose second element contains
    nodes as keys that map to their parent node in the search tree.
    """

    time: Dict[str, float] = {v: inf for v in graph.vertices()}
    parent: Dict[str, str] = {}

    parent[start] = None

    stack: List[str] = []
    stack.append(start)

    t = 0
    while len(stack) > 0:
        current = stack.pop()
        time[current] = t
        t += 1

        for next in graph.neighbours(current):
            if next not in parent:
                parent[next] = current

                stack.append(next)

    return (time, parent)


V: List[str] = ['1', '2', '3', '4', '5', '6', '7', '8']
E: List[Tuple[str, str]] = [('8', '3'), ('8', '4'), ('8', '5'),
                            ('3', '1'), ('3', '2'), ('4', '6'), ('5', '7')]

G: Graph = Graph(len(V))

for (u, v) in E:
    G.link(u, v)

D, T = bfs(G, '8')
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
