import sys

for line in sys.stdin.readlines():
    sp = line.split(',')
    print(','.join(sp[:-1]) + ',' + str(float(sp[-1])/1000))
