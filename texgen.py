import pandas as pd
import numpy as np

# Read the CSV files
benchmark_data = pd.read_csv('benchresults/benchmarks.csv')
performance_data = pd.read_csv('benchresults/comparison.csv')

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
column_order = ['size', 'type'] + [sys for sys in systems]
print(column_order)
# Reorder the columns
pivot_table = pivot_table.reindex(column_order, axis=1)
pivot_table.reset_index(inplace=True)

# Convert to LaTeX table
latex_table = pivot_table.to_latex(index=False, na_rep='n/a')

# Save the LaTeX table to a .tex file
with open('benchresults/comparison_table.tex', 'w') as f:
    f.write(latex_table)

import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

# Set Seaborn style for minimalistic design
sns.set(style='white')

# Function to convert size from string to numeric (assuming MB)
def convert_size_to_mb(size_str):
    try:
        return float(size_str.replace('MB', ''))
    except ValueError:
        return pd.np.nan

# Apply the conversion to the 'size' column
merged_data['size'] = merged_data['size'].apply(convert_size_to_mb)

# Handle timeouts for scatterplot
# merged_data['time'] = pd.to_numeric(merged_data['time'], errors='coerce')

# Replace NaNs in the 'time' column with the maximum time
max_time = 600
merged_data['time'] = merged_data['time'].fillna(max_time)
print(merged_data)

# Create pivot table of katch vs frenetic
katch_frenetic_pivot = merged_data.pivot_table(
    index='file',
    columns='system',
    values='time',
    aggfunc='mean',
    fill_value='n/a'
)
print(katch_frenetic_pivot)

# Scatterplot for Katch vs Frenetic using Seaborn
plt.figure(figsize=(5, 5))
sns.scatterplot(x='katch', y='frenetic', data=katch_frenetic_pivot)
plt.title('KATch vs Frenetic')
plt.xlabel('KATch Time (s)')
plt.ylabel('Frenetic Time (s)')
# set both axes from 0 to max_time
plt.xlim(0, max_time+10)
plt.ylim(0, max_time+10)
# add dashed diagonal line
plt.plot([0, max_time], [0, max_time], 'k--')
plt.savefig('benchresults/katch_vs_frenetic_scatterplot.pdf', format='pdf', bbox_inches='tight')

# Combined Scatterplot for Time vs Size for both systems using Seaborn
plt.figure(figsize=(5, 5))
sns.scatterplot(x='size', y='time', hue='system', data=merged_data)
plt.title('Time vs Size for Each System')
plt.xlabel('Size (MB)')
plt.ylabel('Time (s)')
plt.legend()
plt.savefig('benchresults/time_vs_size_combined.pdf', format='pdf', bbox_inches='tight')

# Clear the current plot to avoid overlap with future plots
plt.clf()