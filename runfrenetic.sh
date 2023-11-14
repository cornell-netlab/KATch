#!/bin/bash

if [ $# -ne 1 ]; then
    printf "usage: ./runfrenetic.sh <kat-directory>\n"
    exit 0
fi

DIR=$1

check_result() {
    expected=$3
    got=$4
    if [ $3 == $4 ]; then
        printf "\e[32msuccess for $1, $2 - expected: $expected (time: "
    else
        # print failure message in red to stderr along with time
        printf "\e[31mfailure for $1, $2 - expected: $expected, got: $got (time: "
    fi
}

check_bisim() {

    # run frenetic dump bisim and capture the output and time
    local output
    ( time frenetic dump bisim $DIR/{$1,$2} | (read output; check_result $1 $2 $3 $output)  | tee /dev/tty ) 2>&1 > /dev/null  | grep real | sed 's/real//' | tr -d '\n'
    printf ")\e[0m\n"
}


while IFS='\n' read line; do
    unset IFS; read -a fields <<< $line
    printf "Running frenetic dump bisim on ${fields[0]}\n"
    check_bisim ${fields[1]} ${fields[2]} ${fields[3]}
done < "$DIR/index.txt"
