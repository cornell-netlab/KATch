#!/bin/bash
hot=results/hotcomparison.csv
res=results/comparison.csv
katchfull=results/apk/katch_results.txt
apkfull=results/apk/apk_results.txt

while read -r i
do
  #            1-1 reach       slicing         full reach
  # name size  katch frenetic  katch frenetic  katch apk

  # name & size
  grep $i sizes.txt  | tr -d '\n';
  printf " & ";

  # 1-1 reachability: katch frenetic
  grep "katch.*fig10.*"$i".*reachability" $hot |  cut -f3 -d, | scripts/avg.py | tr -d '\n';
  printf " & ";
  grep "frenetic.*fig10.*"$i".*reachability" $res |  cut -f3 -d, | scripts/avg.py | tr -d '\n';
  printf " & ";

  # slicing: katch frenetic
  grep "katch.*fig10.*"$i".*slicing" $hot |  cut -f3 -d, | scripts/avg.py | tr -d '\n';
  printf " & ";
  grep "frenetic.*fig10.*"$i".*slicing" $res |  cut -f3 -d, | scripts/avg.py | tr -d '\n';
  printf " & ";

  # full reachability: katch apk
  if [ "$i" = "Belnet" ]; then i=Belnet2003; fi
  if [ "$i" = "Arpa" ]; then i=Arpanet19728; fi
  grep $i $katchfull | cut -f3 -d' ' | scripts/ms2s.py | scripts/avg.py  | tr -d '\n' | tr -d '\\';
  printf " & ";
  grep $i $apkfull | cut -f5 -d' '  | scripts/ms2s.py | scripts/avg.py | tr -d '\n';
  printf "\\\\\\\\\n";
done <<< 'Layer42
Compuserv
Airtel
Belnet
Shentel
Arpa
Sanet
Uunet
Missouri
Telcove
Deltacom
Cogentco
Kdl'
