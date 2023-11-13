#!/usr/bin/python3
import sys

if len(sys.argv) < 2:
    print("usage: gen-lin.py n")
    sys.exit(1)

n = int(sys.argv[1])

bigstart = "@st=N0⋅@sw=N0"
sum1 = "@sw=N0"
sum2 = "@st=N0"

# print backward ((exists @pt (exists @dst (forward bigstart⋅(main⋅top⋅δ)⋆))) ^ bigfinish)

for i in range(1,n):
    bigstart += f' ∪ @st=N{i}⋅@sw=N{i}'
    sum1 += f' ∪ @sw=N{i}'
    sum2 += f' ∪ @st=N{i}'

print(f'bigstart = {bigstart}')
print(f'sum1 = {sum1}')
print(f'sum2 = {sum2}')
print("check forward (exists @pt exists @dst forward bigstart⋅(main⋅top⋅δ)⋆) ≡ sum1⋅sum2")
