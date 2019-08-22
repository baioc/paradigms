from graphs import Graph, Digraph
from math import inf


def grprint(g: Graph):
	print("Graph has a total of", g.node_number(), "nodes and", g.edge_number(), "edges")
	for a in g.nodes():
		print("Degree of", a, "is", g.degree(a), "(out: {})".format(g.degree_out(a)), "(in: {})".format(g.degree_in(a)))
		for b in g.neighbours(a):
			print("  |->", b, "(w: {})".format(g.weight(a,b)))


V = ['a', 'b', 'c', 'd', 'e']
E = [('a','b'), ('b','c',2), ('c','d',-3.25), ('d','b',3.75), ('d','e'), ('e','c',0.0)]

G = Digraph()

grprint(G)
print("")


for v in V:
	print(G.insert(v))

print(G.insert('a'))

grprint(G)
print("")


for e in E:
	if len(e) == 2:
		x, y = e
		print(G.link(x, y))
	elif len(e) == 3:
		x, y, w = e
		print(G.link(x, y, w))

grprint(G)
print("")


print(G.edge_number())
# next two lines should be equivalent
# print(G.link('a', 'b', inf))
print(G.unlink('a', 'b'))
print(G.edge_number())
print(G.weight('a', 'b'))

grprint(G)
print("")


print(G.erase('d'))

grprint(G)
# print("")
