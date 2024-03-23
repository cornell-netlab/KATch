#!/usr/bin/env python3

import sys

for line in sys.stdin.readlines():
    n = float(line)/1000
    print(f'{n:.2f}')
