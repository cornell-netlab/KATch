import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import re
import os

def get_file_size(filepath):
    """
    Recursively computes the size of a file and all files it imports.
    The import paths are relative to the file itself.
    """
    total_size = 0
    files_processed = set()

    def process_file(file):
        if file in files_processed:
            return 0
        files_processed.add(file)

        try:
            with open(file, 'r') as f:
                contents = f.read()
            size = os.path.getsize(file) / 1000.0
        except IOError:
            print(f"\033[91mError reading file: {file}")
            return 0

        # Add the size of the current file
        nonlocal total_size
        total_size += size

        # Find and process all imported files
        for line in contents.split('\n'):
            match = re.match(r'import "(.+?)"', line)
            if match:
                # Construct path relative to the current file
                imported_file = os.path.join(os.path.dirname(file), match.group(1))
                process_file(imported_file)

    process_file(filepath)
    return total_size

def group_fn(path):
    if re.search("small|medium|large", path):
        return "Topology Zoo"
    if re.search("topo-zoo", path):
        return "Full reachability"
    if re.search("inc", path):
        return "Inc"
    if re.search("flip", path):
        return "Flip"
    if re.search("nondet", path):
        return "Non-determinism"
    print(f'No group for {path}!')
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

# Process each line
for line in data.split('\n'):
    if not line: continue
    system, path, time = line.split(',')
    path_parts = path.split('/')
    # group = path_parts[2]
    # path_parts[-1] on on "-"" and "_" using regex
    name_type = re.split(r'[-_]', path_parts[-1])
    name = name_type[0].split('.')[0]
    type = name_type[-1].split('.')[0]
    if type not in ['slicing', 'reachability']:
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
    sizes.append(get_file_size(path))

# Create DataFrame
df = pd.DataFrame({
    'group': groups,
    'name': names,
    'type': types,
    'size': sizes,
    'system': systems,
    'time': times,
    'timeout': timeouts
})

# Display the DataFrame

# Filter data for 'frenetic' and 'katch'
frenetic_data = df[df['system'] == 'frenetic']
katch_data = df[df['system'] == 'katch']

# Group by 'name', 'type', and 'group', then calculate the mean time
frenetic_avg = frenetic_data.groupby(['name', 'type', 'group'])['time'].mean().reset_index()
katch_avg = katch_data.groupby(['name', 'type', 'group'])['time'].mean().reset_index()

# Merge the averaged data
merged_data = pd.merge(frenetic_avg, katch_avg, on=['name', 'type', 'group'], suffixes=('_frenetic', '_katch'))

# Plotting

# Scatterplots katch vs frenetic
for group in merged_data['group'].unique():
    group_data = merged_data[merged_data['group'] == group]

    plt.figure(figsize=(5, 5))
    sns.scatterplot(data=group_data, x='time_frenetic', y='time_katch')
    plt.title(f"Scatter Plot for Group: {group} (Averaged Times)")
    plt.xlabel("Average Time (Frenetic)")
    plt.ylabel("Average Time (Katch)")
    plt.grid(True)
    plt.savefig(f'plots/{group}_scatter.pdf', bbox_inches='tight', format='pdf')

# Scatterplots time vs size
for group in df['group'].unique():
    group_data = df[df['group'] == group]

    plt.figure(figsize=(5, 5))
    sns.scatterplot(data=group_data, x='size', y='time', hue='system', style='system', markers=True)
    plt.title(f"Size vs Time for Group: {group}")
    plt.xlabel("Size")
    plt.ylabel("Time")
    plt.grid(True)
    plt.legend(title='System')
    plt.savefig(f'plots/{group}_time_vs_size.pdf', bbox_inches='tight', format='pdf')

# Latex tables for katch vs frenetic
for group in df['group'].unique():
    group_data = df[df['group'] == group]

    # Selecting the required columns
    selected_columns = ['name', 'type', 'size', 'system', 'time']
    table_df = group_data[selected_columns]

    # Splitting the data based on system
    katch_df = table_df[table_df['system'] == 'katch']
    frenetic_df = table_df[table_df['system'] == 'frenetic']

    # Merging on 'name', 'type', and 'size'
    merged_df = pd.merge(katch_df, frenetic_df, on=['name', 'type', 'size'], suffixes=('_katch', '_frenetic'))

    # Selecting and renaming columns for the final table
    final_table_df = merged_df[['name', 'type', 'size', 'time_katch', 'time_frenetic']]

    # drop 'type' column if it only contains "none"
    if list(final_table_df['type'].unique()) == ['none']:
        final_table_df = final_table_df.drop(columns=['type'])

    # Sorting by 'size'
    final_table_df = final_table_df.sort_values(by='size')

    # Make table headers read File, Size (KB), KATch, Frenetic
    final_table_df.rename(columns={'size': 'Size (KB)', 'time_katch': 'KATch (s)', 'time_frenetic': 'Frenetic (s)'}, inplace=True)

    # Convert to LaTeX format
    # latex_table = final_table_df.to_latex(index=False)

    # Convert to LaTeX table
    latex_table = final_table_df.to_latex(index=False, na_rep='n/a', float_format='%.2f')

    # Save the LaTeX table to a .tex file
    with open(f'plots/{group}_comparison_table.tex', 'w') as f:
        f.write(latex_table)

