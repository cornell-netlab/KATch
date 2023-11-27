#!/usr/bin/python3
import sys,subprocess

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
    children = [[]]
    lastrow = 0

    for i in range(depth):
        end = len(children)
        for j in range(lastrow, end):
            for k in range(arity):
                children[j].append(len(children))
                children.append([])
        lastrow = end
    return children

def print_dot(tree, d, a, f):
    with open(f, 'w+') as f:
        print(f'strict digraph ft_{d}_{a} {{', file=f)
        for i in range(len(tree)):
            sw_attr = f'id={i}, type=switch'
            host_attr = f'id={i}, ip="192.168.1.{i}", mac="00:00:00:00:00:{i}", type=host'
            print(f'  s{i} [{sw_attr}];', file=f)
            print(f'  h{i} [{host_attr}];', file=f)
            print(f'  s{i} -> h{i} [{link_attr}]', file=f)
            print(f'  h{i} -> s{i} [{link_attr}]', file=f)
        for i,children in enumerate(tree):
            for j,c in enumerate(children):
                print(f'  s{i} -> s{c} [{link_attr}]', file=f)
                print(f'  s{c} -> s{i} [{link_attr}]', file=f)
        print('}', file=f)


if __name__ == "__main__":
    subprocess.run(["mkdir", "-p", pdfs])

    depth = int(sys.argv[1])
    arity = int(sys.argv[2])
    bn = f'/ft-{depth}-{arity}'
    dotfile = ftdir + bn + '.dot'
    t = gen_tree(depth, arity)
    # print(t)
    print_dot(t, depth, arity, dotfile)

    out = open(pdfs + bn + '.pdf','w+')
    subprocess.run(["dot", dotfile, "-Tpdf"], stdout=out)
