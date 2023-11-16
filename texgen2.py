import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import re
import os
import sys

def get_file_size(file):
    try:
        with open(file, 'r') as f:
            contents = f.read()
    except IOError:
        print(f"\033[91mError reading file: {file}")
        return 0

    total_size = 0
    # if combinatorial in the name, include the file size itself
    if "inc" in file or "flip" in file or "nondet" in file:
        print(f"combinatorial in {file}")
        total_size += os.path.getsize(file) / 1000.0
        return total_size
        print(f"total size: {total_size}")

    # Find and process all imported files
    for line in contents.split('\n'):
        match = re.match(r'import "(.+?)"', line)
        if match:
            # Construct path relative to the current file
            imported_file = os.path.join(os.path.dirname(file), match.group(1))
            size = os.path.getsize(imported_file) / 1000.0
            total_size += size

    return total_size

def group_fn(path):
    if re.search("small|medium|large", path):
        return "Topology Zoo"
    if re.search("linear-reach", path):
        print("ERRROR")
        sys.exit(1)
        return "Linear reachability"
    if re.search("topo-zoo", path):
        return "Full reachability"
    if re.search("inc", path):
        return "Inc"
    if re.search("flip", path):
        return "Flip"
    if re.search("nondet", path):
        return "Nondet"
    print(f'No group for {path}!')
    return "misc"
    sys.exit(1)

# Sample data
data = open('benchresults/comparison.csv').read()


# Initialize lists to hold the extracted data
systems = []
groups = []
names = []
types = []
times = []
timeouts = []
sizes = []
algs = []

# Process each line
for line in data.split('\n'):
    if not line: continue
    system, path, time = line.split(',')
    size = get_file_size(path)
    # If "linear-reachability" is in the path, replace system with "katch (linear)", and replace "linear-reachability" with "naive-reachability"
    alg = "naive"
    if "linear-reachability" in path:
        system = "katch"
        path = path.replace("linear-reachability", "naive-reachability")
        alg = "linear"
    path_parts = path.split('/')
    # group = path_parts[2]
    # path_parts[-1] on on "-"" and "_" using regex
    name_type = re.split(r'[-_]', path_parts[-1])
    name = name_type[0].split('.')[0]
    type = name_type[-1].split('.')[0]
    if alg == "linear":
        type = "reachability"
    if type not in ['slicing', 'reachability', 'unreachability']:
        type = 'none'

    # Handle timeout
    timeout = 'timeout' in time
    if timeout:
        # Extract the timeout value (assuming it's always in seconds)
        time = float(re.search(r'\((\d+)s\)', time).group(1))
    else:
        time = float(time)

    # Append to lists
    systems.append(system)
    groups.append(group_fn(path))
    names.append(name)
    types.append(type)
    times.append(time)
    timeouts.append(timeout)
    sizes.append(size)
    algs.append(alg)
    # if alg is linear, print all fields
    # if alg == "linear":
        # print(f"{system}, {group_fn(path)}, {name}, {type}, {size}, {time}, {timeout}, {alg}")

# Create DataFrame
df = pd.DataFrame({
    'group': groups,
    'name': names,
    'type': types,
    'size': sizes,
    'system': systems,
    'time': times,
    'timeout': timeouts,
    'alg': algs
})

# write dataframe to csv out.csv
df.to_csv('out.csv')

# Display the DataFrame

# Filter data for 'frenetic' and 'katch'
frenetic_data = df[df['system'] == 'frenetic']
katch_data = df[df['system'] == 'katch']
print(katch_data['alg'].unique())

# Group by 'name', 'type', and 'group', then calculate the mean time
frenetic_avg = frenetic_data.groupby(['name', 'type', 'group', 'size', 'alg'])['time'].mean().reset_index()
katch_avg = katch_data.groupby(['name', 'type', 'group', 'size', 'alg'])['time'].mean().reset_index()
print(katch_avg['alg'].unique())

merged_data = pd.merge(frenetic_avg, katch_avg, on=['name', 'type', 'group', 'size'], suffixes=('_frenetic', '_katch'))

print(merged_data['alg_katch'].unique())
# exit()
# Plotting

sizes = {'': (3,3), '_wide': (10, 4)}
system_color_symbols = {
    'frenetic': ('#1f77b4', 'o'),
    'frenetic (timeout)': ('#000', 'X'),
    'katch': ('#ff7f0e', 'o'),
    'katch (linear)': ('#2ca02c', 'o'),
}

palette = {system: color for system, (color, marker) in system_color_symbols.items()}
markers = {system: marker for system, (color, marker) in system_color_symbols.items()}


# Scatterplots katch vs frenetic
for group in merged_data['group'].unique():
    group_data = merged_data[merged_data['group'] == group]

    for sizename, size in sizes.items():
        plt.figure(figsize=size)
        sns.scatterplot(data=group_data, x='time_frenetic', y='time_katch')
        plt.title(f"{group}")
        plt.xlabel("Time (Frenetic)")
        plt.ylabel("Time (KATch)")
        plt.grid(True)
        plt.savefig(f'plots/{group}_scatter{sizename}.pdf', bbox_inches='tight', format='pdf')

# Scatterplots time vs size
for group in merged_data['group'].unique():
    group_data = merged_data[merged_data['group'] == group]
    # split group_data into separate rows for frenetic and katch
    frenetic_data = group_data[['size', 'time_frenetic']].copy()
    frenetic_data.rename(columns={'time_frenetic': 'time'}, inplace=True)
    frenetic_data['system'] = 'frenetic'
    katch_data = group_data[['size', 'time_katch', 'alg_katch']].copy()
    katch_data.rename(columns={'time_katch': 'time'}, inplace=True)
    katch_data['system'] = 'katch'
    print(katch_data['alg_katch'].unique())

    # combine frenetic and katch data
    group_data_kf = pd.concat([frenetic_data, katch_data])

    # Add a separate system "frenetic (timeout)" for the timeouts
    group_data_kf['system'] = group_data_kf.apply(lambda row: f"{row['system']} (timeout)" if row['time'] == 300 else row['system'], axis=1)
    group_data_kf['system'] = group_data_kf.apply(lambda row: f"{row['system']} (linear)" if row['alg_katch'] == 'linear' else row['system'], axis=1)

    for sizename, size in sizes.items():
        plt.figure(figsize=size)
        # sns.scatterplot(data=group_data_kf, x='size', y='time', hue='system', style='system', markers=True)
        sns.scatterplot(data=group_data_kf, x='size', y='time', hue='system', style='system', palette=palette, markers=markers)
        plt.title(f"{group}")
        plt.xlabel("Size (KB)")
        plt.ylabel("Time (s)")
        plt.grid(True)
        plt.legend(title='System')
        plt.savefig(f'plots/{group}_time_vs_size{sizename}.pdf', bbox_inches='tight', format='pdf')

# Latex tables for katch vs frenetic
for group in merged_data['group'].unique():
    group_data = merged_data[merged_data['group'] == group]

    # Selecting and renaming columns for the final table
    final_table_df = group_data[['name', 'type', 'size', 'time_katch', 'time_frenetic']]

    # drop 'type' column if it only contains "none"
    if list(final_table_df['type'].unique()) == ['none']:
        final_table_df = final_table_df.drop(columns=['type'])

    # Remove rows with name ft2
    final_table_df = final_table_df[final_table_df['name'] != 'ft2']

    final_table_df['sort_key'] = final_table_df.apply(lambda row: row['name'].startswith("ft"), axis=1)

    # print(final_table_df)

    # Sorting by 'size'
    final_table_df = final_table_df.sort_values(by=['sort_key', 'size']).drop('sort_key', axis=1)

    # Make table headers read File, Size (KB), KATch, Frenetic
    final_table_df.rename(columns={'name': 'Name', 'type': 'Type', 'size': 'Size (KB)', 'time_katch': 'KATch (s)', 'time_frenetic': 'Frenetic (s)'}, inplace=True)

    # Calculate speedup, round to integer
    final_table_df['Speedup'] = (final_table_df['Frenetic (s)'] / final_table_df['KATch (s)']).round().astype(int)

    # print(final_table_df)

    # Convert to LaTeX format
    # latex_table = final_table_df.to_latex(index=False)

    # Convert to LaTeX table
    latex_table = final_table_df.to_latex(index=False, na_rep='n/a', float_format='%.2f')

    # Save the LaTeX table to a .tex file
    with open(f'plots/{group}_comparison_table.tex', 'w') as f:
        f.write(latex_table)

# Get the current working directory
cwd = os.getcwd()
if "jules" in cwd:
    # Copy the plots directory to /Users/jules/git/misc/5stars/writeups/pldi24/plots
    os.system("cp -r plots /Users/jules/git/misc/5stars/writeups/pldi24/")
