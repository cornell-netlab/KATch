def process_line(line):
    # Split the line at the '≡' symbol
    part1, part2 = line.split('≡')

    # Remove the 'check' from part1
    part1 = part1.replace('check ', '').strip()

    # Create the four variations
    variations = [
        f"check (forward (({part1})^({part2}))) ≡ ∅",
        f"check (backward (({part1})^({part2}))) ≡ ∅",
        f"check (forward (({part1})-({part2}))) ≡ ∅",
        f"check (backward (({part1})-({part2}))) ≡ ∅"
    ]

    return variations

def process_file(input_file, output_file):
    n = 0
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            n += 1
            print(n)
            # Ignore empty lines
            if line.strip():
                variations = process_line(line.strip())
                for var in variations:
                    outfile.write(var + '\n')

# Replace 'input.txt' with the path to your input file and 'output.txt' with your desired output file path.
process_file('nkpl/scratch/fuzz100k.nkpl', 'nkpl/scratch/fuzzfb400k.nkpl')
