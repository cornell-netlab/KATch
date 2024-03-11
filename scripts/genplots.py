import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import re
import os
import sys

input_file = 'benchresults/comparison.csv' if len(sys.argv) == 1 else sys.argv[1]
output_dir = 'results/plots'

def size_fn(path):
    # Count the number of atoms (=, ≠, δ, ←) in the file
    with open(path, 'r') as f:
        contents = f.read()
    return contents.count('≠') + contents.count('=') + contents.count('δ') + contents.count('←')

# Compute file sizes for the benchmarks
def get_file_size(file):
    try:
        with open(file, 'r') as f:
            contents = f.read()
    except IOError:
        print(f"\033[91mError reading file: {file}")
        return 0

    total_size = 0
    if "inc" in file or "flip" in file or "nondet" in file:
        total_size += size_fn(file)

    # Find and process all imported files
    for line in contents.split('\n'):
        match = re.match(r'import "(.+?)"', line)
        if match:
            # Construct path relative to the current file
            imported_file = os.path.join(os.path.dirname(file), match.group(1))
            total_size += size_fn(imported_file)

    return total_size

def group_fn(path):
    if re.search("fig10", path):
        return "Topology Zoo"
    if re.search("linear-reach", path):
        print("ERRROR")
        sys.exit(1)
        return "Linear reachability"
    if re.search("naive-reach", path):
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

data = open(input_file).read()

systems = []
groups = []
names = []
types = []
times = []
timeouts = []
sizes = []
algs = []

timeout_used = None

for line in data.split('\n'):
    if not line: continue
    system, path, time = line.split(',')
    size = get_file_size(path)
    alg = "naive"
    if "linear-reachability" in path:
        system = "katch"
        path = path.replace("linear-reachability", "naive-reachability")
        alg = "linear"
    path_parts = path.split('/')
    name_type = re.split(r'[-_]', path_parts[-1])
    name = name_type[0].split('.')[0]
    type = name_type[-1].split('.')[0]
    if alg == "linear":
        type = "reachability"
    if type not in ['slicing', 'reachability', 'unreachability']:
        type = 'none'

    timeout = 'timeout' in time
    if timeout:
        # Extract the timeout value (assuming it's always in seconds)
        time = float(re.search(r'\((\d+)s\)', time).group(1))
        if timeout_used and time != timeout_used:
            print("Inconsistent timeouts! Restart with clean comparison.csv.")
            sys.exit(1)
        else:
            timeout_used = time
    else:
        time = float(time)

    systems.append(system)
    groups.append(group_fn(path))
    names.append(name)
    types.append(type)
    times.append(time)
    timeouts.append(timeout)
    sizes.append(size)
    algs.append(alg)

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

df.to_csv('out.csv')

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

sizes = {'': (3,3), '_wide': (10, 4)}
system_color_symbols = {
    'frenetic': ('#1f77b4', 'o'),
    'frenetic (timeout)': ('#000', 'X'),
    'katch': ('#ff7f0e', 'D'),
    'katch (linear)': ('#2ca02c', 'd'),
    'katch (timeout)': ('#666666', '*'),
    'katch (linear) (timeout)': ('#666666', '*'),
}

palette = {system: color for system, (color, marker) in system_color_symbols.items()}
markers = {system: marker for system, (color, marker) in system_color_symbols.items()}


# Scatterplots katch vs frenetic
for group in merged_data['group'].unique():
    group_data = merged_data[merged_data['group'] == group]

    def mk_plot(name, size):
        plt.figure(figsize=size)
        sns.scatterplot(data=group_data, x='time_frenetic', y='time_katch')
        if group != "Topology Zoo": plt.title(f"{group}")
        plt.xlabel("Time (Frenetic)")
        plt.ylabel("Time (KATch)")
        plt.grid(True)
        plt.savefig(f'{output_dir}/{name}.pdf', bbox_inches='tight', format='pdf')

    if group == 'Topology Zoo':
        mk_plot('Fig10a', (3,3))
    

print(f'timeout: {timeout_used}')

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

    if group == 'Full reachability':
        # Clip times for "katch" at the timeout
        katch_data['time'] = katch_data.apply(lambda r: timeout_used if r['time'] >= timeout_used else r['time'], axis=1)

    # combine frenetic and katch data
    group_data_kf = pd.concat([frenetic_data, katch_data])

    # Add a separate system "[system] (timeout)" for the timeouts (Full
    # reachability only)
    if group == 'Full reachability':
        group_data_kf['system'] = group_data_kf.apply(lambda row: f"{row['system']} (linear)" if row['alg_katch'] == 'linear' else row['system'], axis=1)
        group_data_kf['system'] = group_data_kf.apply(lambda row: f"{row['system']} (timeout)" if row['time'] >= timeout_used else row['system'], axis=1)

    def mk_plot(name, size):
        plt.figure(figsize=size)
        sns.scatterplot(data=group_data_kf, x='size', y='time', hue='system', style='system', palette=palette, markers=markers)
        if group != "Topology Zoo": plt.title(f"{group}")
        plt.xlabel("Size (atoms)")
        plt.ylabel("Time (s)")
        plt.grid(True)
        plt.legend(title='System')
        # plt.savefig(f'plots/{group}_time_vs_size{sizename}.pdf', bbox_inches='tight', format='pdf')
        plt.savefig(f'{output_dir}/{name}.pdf', bbox_inches='tight', format='pdf')

    if group == 'Full reachability':
        mk_plot('Fig09', (10,4))
    elif group == 'Topology Zoo':
        mk_plot('Fig10b', (3,3))
    elif group == 'Inc':
        mk_plot('Fig11-inc', (3,3))
    elif group == 'Flip':
        mk_plot('Fig11-flip', (3,3))
    elif group == 'Nondet':
        mk_plot('Fig11-nondet', (3,3))

# fig09:
# Full reachability_time_vs_size_wide.pdf

# fig10:
# Topology Zoo_scatter.pdf
# Topology Zoo_time_vs_size.pdf

# fig11:
# Inc_time_vs_size.pdf
# Nondet_time_vs_size.pdf
# Flip_time_vs_size.pdf

# Latex tables for katch vs frenetic

def mk_table(group, name):
    group_data = merged_data[merged_data['group'] == group]

    # Selecting and renaming columns for the final table
    final_table_df = group_data[['name', 'type', 'size', 'time_katch', 'time_frenetic']]

    # drop 'type' column if it only contains "none"
    if list(final_table_df['type'].unique()) == ['none']:
        final_table_df = final_table_df.drop(columns=['type'])

    # Sorting by 'size'
    final_table_df = final_table_df.sort_values(by=['size'])

    # Make table headers read File, Size (KB), KATch, Frenetic
    final_table_df.rename(columns={'name': 'Name', 'type': 'Type', 'size': 'Size', 'time_katch': 'KATch (s)', 'time_frenetic': 'Frenetic (s)'}, inplace=True)

    # Calculate speedup, round to integer
    final_table_df['Speedup'] = (final_table_df['Frenetic (s)'] / final_table_df['KATch (s)']).round().astype(int)

    latex_table = final_table_df.to_latex(index=False, na_rep='n/a', float_format='%.2f')

    with open(f'{output_dir}/{name}_table.tex', 'w') as f:
        f.write(latex_table)

mk_table('Topology Zoo', 'Fig10')
