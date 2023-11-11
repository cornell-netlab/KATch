
#!/bin/bash

check_bisim() {
    # Assign arguments to variables
    local file1=$1
    local file2=$2
    local expected_output=$3

    # Run frenetic dump bisim and capture the output and time
    local output
    local time_info
    time_info=$( { time frenetic dump bisim "$file1" "$file2" 2>&1 1>&3 3>&- | sed 's/real//'; } 3>&1 )

    # Compare the output with the expected output and print a single line result
    if [ "$output" == "$expected_output" ]; then
        # Print success message in green along with time
        echo -e "\e[32mSuccess for $file1, $file2 - Expected: $expected_output (Time: $time_info)\e[0m"
    else
        # Print failure message in red to stderr along with time
        echo -e "\e[31mFailure for $file1, $file2 - Expected: $expected_output, Got: $output (Time: $time_info)\e[0m" >&2
    fi
}

echo "Running frenetic dump bisim on benchmarks/tiny/Airtel_unreachability.nkpl"
check_bisim 63154970.kat 49f887f2.kat true
echo "Running frenetic dump bisim on benchmarks/tiny/Belnet_unreachability.nkpl"
check_bisim 40128853.kat 49f887f2.kat true
echo "Running frenetic dump bisim on benchmarks/tiny/Compuserv_unreachability.nkpl"
check_bisim c5659df2.kat 49f887f2.kat true
echo "Running frenetic dump bisim on benchmarks/tiny/Layer42_unreachability.nkpl"
check_bisim 1d40a8a8.kat 49f887f2.kat true
echo "Running frenetic dump bisim on benchmarks/tiny/t4_h_reachability.nkpl"
check_bisim cec2b1a0.kat 49f887f2.kat false
echo "Running frenetic dump bisim on benchmarks/tiny/t4_near_reachability.nkpl"
check_bisim 6dbfd4c.kat 49f887f2.kat false
echo "Running frenetic dump bisim on benchmarks/tiny/t4_reachability.nkpl"
check_bisim d7dd4e9e.kat 49f887f2.kat false
echo "Running frenetic dump bisim on benchmarks/tiny/t4_slicing.nkpl"
check_bisim 916bf443.kat cd88356b.kat true
