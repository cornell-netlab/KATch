import pandas as pd
import numpy as np
import os

# Read the CSV files
performance_data = pd.read_csv('benchresults/comparison.csv')

import re

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
            print(f"Error reading file: {file}")
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

# Make a benchmark_data dataframe with the following structure:
# file, size
benchmark_data = []
# Look up all the file sizes for files that occur in performance_data
for file in performance_data['file'].unique():
    # Get the file size from disk
    size = get_file_size(file)

    # Add the file and size to the benchmark_data dataframe
    benchmark_data.append({'file': file, 'size': size})

# Convert benchmark_data to a dataframe
benchmark_data = pd.DataFrame(benchmark_data)

# Replace 'timeout' with NaN in performance data for calculation purposes
performance_data['time'] = pd.to_numeric(performance_data['time'], errors='coerce')

# Group by system and file, calculating the average time
grouped_performance_data = performance_data.groupby(['system', 'file']).mean().reset_index()
print(grouped_performance_data)

# Merge the benchmark data with the performance data
merged_data = pd.merge(grouped_performance_data, benchmark_data, on='file', how='right')
print(merged_data)

# Pivot the table for LaTeX formatting
pivot_table = merged_data.pivot_table(
    index='file',
    columns='system',
    values='time',
    aggfunc='mean',
    fill_value='n/a'
)
print(pivot_table)

# Add size and type columns from benchmark data
pivot_table = pivot_table.join(benchmark_data.set_index('file'))
print(pivot_table)

# Reorder columns (if necessary)
# Get the unique system names and sort them
systems = np.sort(performance_data['system'].unique())
# Create a new column order
column_order = ['size'] + [sys for sys in systems]
print(column_order)
# Reorder the columns
pivot_table = pivot_table.reindex(column_order, axis=1)
pivot_table.reset_index(inplace=True)
# Sort the table by size
pivot_table.sort_values(by=['size'], inplace=True)
# Round the size column to 1 decimal place
pivot_table['size'] = pivot_table['size']
# Round the time columns to 2 decimal places
for sys in systems:
    pivot_table[sys] = pivot_table[sys]

# Make table headers read File, Size (KB), KATch, Frenetic
pivot_table.rename(columns={'file': 'File', 'size': 'Size (KB)', 'katch': 'KATch (s)', 'frenetic': 'Frenetic (s)'}, inplace=True)

# Convert to LaTeX table
latex_table = pivot_table.to_latex(index=False, na_rep='n/a', float_format='%.2f')

# Save the LaTeX table to a .tex file
with open('benchresults/comparison_table.tex', 'w') as f:
    f.write(latex_table)

import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

# Set Seaborn style for minimalistic design
sns.set(style='white')

# Handle timeouts for scatterplot
# merged_data['time'] = pd.to_numeric(merged_data['time'], errors='coerce')

# Replace NaNs in the 'time' column with the maximum time
max_time = 600
merged_data['time'] = merged_data['time'].fillna(max_time)

# Create pivot table of katch vs frenetic
katch_frenetic_pivot = merged_data.pivot_table(
    index='file',
    columns='system',
    values='time',
    aggfunc='mean',
    fill_value='n/a'
)

print(f"{katch_frenetic_pivot=}")

# Scatterplot for Katch vs Frenetic using Seaborn
plt.figure(figsize=(5, 5))
sns.scatterplot(x='katch', y='frenetic', data=katch_frenetic_pivot)
plt.title('KATch vs Frenetic')
plt.xlabel('KATch Time (s)')
plt.ylabel('Frenetic Time (s)')
# set both axes from 0 to max_time
# plt.xlim(0, max_time+10)
# plt.ylim(0, max_time+10)
# add dashed diagonal line
# plt.plot([0, max_time], [0, max_time], 'k--')
plt.savefig('benchresults/katch_vs_frenetic_scatterplot.pdf', format='pdf', bbox_inches='tight')

# Combined Scatterplot for Time vs Size for both systems using Seaborn
plt.figure(figsize=(5, 5))
sns.scatterplot(x='size', y='time', hue='system', data=merged_data)
plt.title('Time vs Size for Each System')
plt.xlabel('Size (KB)')
plt.ylabel('Time (s)')
plt.legend()
plt.savefig('benchresults/time_vs_size_combined.pdf', format='pdf', bbox_inches='tight')

# Clear the current plot to avoid overlap with future plots
plt.clf()
