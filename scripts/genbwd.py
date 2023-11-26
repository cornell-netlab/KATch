#!/usr/bin/python3
import sys,re

def process_line(line):

    op = ''
    if re.search('≡', line):
        op = '≡'
    elif re.search('≢', line):
        op = '≢'
    else:
        return line.strip()

    # Split the line at the '≡' symbol
    part1, part2 = line.split(op)
    part1 = part1.replace('check ', '').strip()

    return f"check (backward (({part1})^({part2}))) {op} ∅"

def process_file(input_file):
    n = 0
    with open(input_file, 'r') as infile:
        for line in infile:
            # n += 1
            # print(n)
            print(process_line(line.strip()))

process_file(sys.argv[1])
