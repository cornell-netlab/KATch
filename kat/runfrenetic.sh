#!/bin/bash

check_bisim() {
    # Assign arguments to variables
    local file1=$1
    local file2=$2
    local expected_output=$3

    # Run frenetic dump bisim and capture the output, time and memory usage
    local cmd_output
    cmd_output=$(/usr/bin/time -f "\nTime: %E\nMemory: %MKB" frenetic dump bisim "$file1" "$file2" 2>&1)

    # Extract the output and the time/memory statistics
    local frenetic_output=$(echo "$cmd_output" | head -n -2)
    local time_memory=$(echo "$cmd_output" | tail -n 2)

    # Compare the frenetic output with the expected output and print a single line result
    if [ "$frenetic_output" == "$expected_output" ]; then
        # Print success message in green along with time and memory usage
        echo -e "\e[32mSuccess for $file1, $file2 - Expected: $expected_output$time_memory\e[0m"
    else
        # Print failure message in red to stderr along with time and memory usage
        echo -e "\e[31mFailure for $file1, $file2 - Expected: $expected_output, Got: $frenetic_output$time_memory\e[0m" >&2
    fi
}

check_bisim 63154970.kat 49f887f2.kat true
check_bisim 40128853.kat 49f887f2.kat true
check_bisim c5659df2.kat 49f887f2.kat true
check_bisim 1d40a8a8.kat 49f887f2.kat true
check_bisim cec2b1a0.kat 49f887f2.kat false
check_bisim 6dbfd4c.kat 49f887f2.kat false
check_bisim d7dd4e9e.kat 49f887f2.kat false
check_bisim 916bf443.kat cd88356b.kat true
