#!/bin/bash

check_bisim() {
    # Assign arguments to variables
    local file1=$1
    local file2=$2
    local expected_output=$3

    # Run frenetic dump bisim and capture the output
    local output=$(frenetic dump bisim "$file1" "$file2")

    # Compare the output with the expected output and print a single line result
    if [ "$output" == "$expected_output" ]; then
        echo -e "\e[32mSuccess for $file1, $file2 - Expected: $expected_output\e[0m"
    else
        # Print failure message in red to stderr
        echo -e "\e[31mFailure for $file1, $file2 - Expected: $expected_output, Got: $output\e[0m" >&2
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
