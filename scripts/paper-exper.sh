#!/bin/bash
./katch compare 300s nkpl/fig09/naive-reachability
./katch run nkpl/fig09/linear-reachability
./katch compare 48h nkpl/fig10
./katch run nkpl/flip-first11
./katch run nkpl/inc-first10
./katch run nkpl/nondet-first15
