from .graphs import Graph, Digraph

from math import inf
from typing import NewType as _NewType

Label = _NewType('Label', str)
Weight = _NewType('Weight', float)

from .search import breadth_first_search, depth_first_search
from .cycle import eulerian_cycle, hamiltonian_circuit
from .path import shortest_path, shortest_route, shortest_network
