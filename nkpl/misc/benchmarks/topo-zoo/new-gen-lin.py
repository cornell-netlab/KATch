#!/usr/bin/python3
import sys,re

if len(sys.argv) < 2:
    print("usage: new-gen-lin.py <topo>")
    sys.exit(1)

topo = sys.argv[1]

lines = open('tops_and_routes/' + topo + '-top.nkpl').readlines()

used_nodes = set()

for i in lines[:-1]:
    node = i.split()[0]
    if re.search(node + '\?', lines[-1]):
        used_nodes.add(node)

print(f'import "../tops_and_routes/{topo}-top.nkpl"')
print(f'import "../tops_and_routes/{topo}-rt.nkpl"')
print("net = (main⋅top⋅δ)⋆")
print("all = ∅", end='')
for i in used_nodes:
    print(f" ∪ @sw={i}", end='')
print('')
for i in used_nodes:
    print(f"check exists @pt (exists @dst (forward (@sw={i} ⋅ net))) ≡ all")

