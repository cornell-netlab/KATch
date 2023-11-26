#!/bin/bash

if [ $# -ne 2 ]; then
    printf "usage: ./runfrenetic.sh <kat-index> <timeout> \n"
    exit 0
fi

timeout -s 9 $2 ./subrunfrenetic.sh $1
exit $?
