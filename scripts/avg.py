#!/usr/bin/env python3
import sys

lines = sys.stdin.readlines()
if lines:
    avg = sum(map(float, lines))/float(len(lines))
    print(f'{avg:#.2f}')
