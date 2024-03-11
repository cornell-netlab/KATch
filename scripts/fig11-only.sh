#!/bin/bash
./katch compare 48h nkpl/fig11/flip-first11 && \
./katch compare 48h nkpl/fig11/inc-first10 && \
./katch compare 48h nkpl/fig11/nondet-first15 && \
python3 scripts/genplots.py
