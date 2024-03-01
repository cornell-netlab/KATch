#!/bin/bash
./katch compare 300s nkpl/fig09/naive-reachability
./katch run nkpl/fig09/linear-reachability
./katch compare 48h nkpl/fig10
./katch compare 48h nkpl/fig11/*-first*
