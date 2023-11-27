#!/bin/bash

if [ $# -ne 2 ]; then
    printf "usage: ./scripts/runfrenetic.sh <kat-index> <timeout> \n"
    exit 0
fi

timeout -s 9 $2 ./scripts/subrunfrenetic.sh $1
exit $?
