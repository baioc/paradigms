import graphs as gr
from math import inf


def grprint(g: gr.Graph):
	print("Graph has a total of", g.vertice_number(), "vertices and", g.edge_number(), "edges")
	for a in g.vertices():
		print("Degree of", a, "is", g.degree(a))
		for b in g.neighbours(a):
			print("  |->", b, "(w: {})".format(g.edges(a)[b])) # g.edges(a)[b] === g.weight(a,b)


V = ['a', 'b', 'c', 'd', 'e']
E = [('a','b'), ('b','c',2), ('c','d',-3.25), ('d','b',3.75), ('d','e'), ('e','c',0.0)]

G = gr.Graph()

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
# print(G.link('b', 'a', inf))
print(G.unlink('b', 'a'))
print(G.edge_number())
print(G.weight('a', 'b'))

grprint(G)
print("")


print(G.erase('d'))

grprint(G)
# print("")
