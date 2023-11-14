#!/usr/bin/python3
import sys

if len(sys.argv) < 2:
    print("usage: gen-lin.py ")
    sys.exit(1)

n = int(sys.argv[1])-1 # subtract 1 because NKPL ranges are inclusive

print(f'net = (main⋅(top) ⋅ δ)⋆')
print(f'all = rangesum @sw 0..{n}')
print(f'for i ∈ 0..{n} do check exists @pt (exists @dst (forward (@sw=i ⋅ net))) ≡ all')





# Notes for big cross product method
"""
bigstart = "@st=N0⋅@sw=N0"
sum1 = "@sw=N0"
sum2 = "@st=N0"

for i in range(1,n):
    bigstart += f' ∪ @st=N{i}⋅@sw=N{i}'
    sum1 += f' ∪ @sw=N{i}'
    sum2 += f' ∪ @st=N{i}'

then
check forward (exists @pt (exists @dst (forward bigstart⋅(main⋅top⋅δ)⋆))) ≡ sum1⋅sum1
"""
