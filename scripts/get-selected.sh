#!/bin/bash
while read i
do
  grep $i results/apk/apk_results.txt | tr -d '\n' | tr - '\\';
  printf " & ";
  grep $i results/apk/katch_results.txt | cut -f 3,4 -d' ' | tr -d '\n';
  printf "\\\\\\\\\n";
done <<< 'Layer42
Compuserve
Airtel
Shentel
Sanet
Uunet
Telcove
Missouri
Deltacom
Cogentco
Kdl'
