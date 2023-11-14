#!/bin/bash

for i in `ls tops_and_routes/*-top.nkpl`; do
  export bn=$(basename $i -top.nkpl)
  echo $bn
  newfile=linear-reachability/$bn.nkpl
  n=$(grep "N[[:digit:]]* =" $i | wc -l)
  head -2 naive-reachability/$bn-reachability.nkpl > $newfile
  ./rch.py $n >> $newfile
done
