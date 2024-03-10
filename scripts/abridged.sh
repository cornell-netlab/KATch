#!/bin/bash
for i in nkpl/fig09/naive-reachability/*; do ./katch compare 60s $i; done && \
./katch run nkpl/fig09/linear-reachability && \
./katch compare 48h nkpl/fig10-less-cogentco && \
./katch compare 48h nkpl/fig11/flip-first11 && \
./katch compare 48h nkpl/fig11/inc-first10 && \
./katch compare 48h nkpl/fig11/nondet-first15 && \
mkdir -p plots
python3 scripts/genplots.py
