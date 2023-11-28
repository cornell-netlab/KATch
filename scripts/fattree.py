#!/usr/bin/python3
import sys,subprocess

graphml_hdr = '<?xml version="1.0" encoding="utf-8"?><graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">'

# Output configs
ftdir = 'fattrees'
pdfs = ftdir + '/pdfs'
nkpl = 'nkpl/fattrees'

# Graph metadata configs
link_attr = 'capacity="1Gbps", cost=1'

if len(sys.argv) < 3:
    print("usage: gen-fattree.py <depth> <arity>")
    sys.exit(1)

def gen_tree(depth, arity):
    adjlist = {}

    def add(i, j):
        if i not in adjlist:
            adjlist[i] = [j]
        else:
            adjlist[i].append(j)

    q = [(0,0)] 
    nxt = 1
    while q[0][0] < depth:
        d, i = q[0]
        q = q[1:]
        for k in range(arity):
            add(i,nxt)
            add(nxt,i)
            q.append((d+1,nxt))
            nxt += 1
    return adjlist

def print_dot(g, name, outfn):
    """ Right now assumes g is undirected; presence of either edge produces both
    edges """
    with open(outfn, 'w+') as f:
        print(f'strict digraph {name} {{', file=f)
        for i in range(len(g)):
            sw_attr = f'id={i}, type=switch'
            host_attr = f'id={i}, ip="192.168.1.{i}", mac="00:00:00:00:00:{i}", type=host'
            print(f'  s{i} [{sw_attr}];', file=f)
            print(f'  h{i} [{host_attr}];', file=f)
            print(f'  s{i} -> h{i} [{link_attr}]', file=f)
            print(f'  h{i} -> s{i} [{link_attr}]', file=f)
        for i,succ in g.items():
            for j,c in enumerate(succ):
                print(f'  s{i} -> s{c} [{link_attr}]', file=f)
                print(f'  s{c} -> s{i} [{link_attr}]', file=f)
        print('}', file=f)

def print_graphml(g, name, outfn):
    with open(outfn, 'w+') as f:
        print(graphml_hdr, file=f)
        print('  <graph id="{name}" edgedefault="undirected"', file=f)
        for i in g.keys():
            print(f' <node id="{i}"/>', file=f)
        for i,j in g.items():
            print(f' <edge source="{i}" target="{j}"', file=f)
        print('  </graph>\n</graphml>', file=f)

if __name__ == "__main__":
    subprocess.run(["mkdir", "-p", pdfs])

    depth = int(sys.argv[1])
    arity = int(sys.argv[2])
    bn = f'ft_{depth}_{arity}'
    dotfile = ftdir + '/' + bn + '.dot'
    g = gen_tree(depth, arity)
    # print(t)
    print_dot(g, bn, dotfile)

    out = open(pdfs + '/' + bn + '.pdf','w+')
    subprocess.run(["dot", dotfile, "-Tpdf"], stdout=out)
