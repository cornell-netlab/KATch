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

import matplotlib.pyplot as plt
import matplotlib.style as style

# Handle timeouts for scatterplot
merged_data['time'] = pd.to_numeric(merged_data['time'], errors='coerce')

# Set the style to minimalist
style.use('seaborn-whitegrid')
plt.rcParams['font.size'] = 12
plt.rcParams['axes.linewidth'] = 1

# Scatterplot for Katch vs Frenetic
katch_times = merged_data[merged_data['system'] == 'katch']['time']
frenetic_times = merged_data[merged_data['system'] == 'frenetic']['time']
plt.figure(figsize=(10, 6))
plt.scatter(katch_times, frenetic_times, alpha=0.7)
plt.title('Katch vs Frenetic')
plt.xlabel('Katch Time')
plt.ylabel('Frenetic Time')
plt.grid(True)
plt.savefig('benchresults/katch_vs_frenetic_scatterplot.pdf', format='pdf', bbox_inches='tight')

# Scatterplot for Time vs Size

# Function to convert size from string to numeric (assuming MB)
def convert_size_to_mb(size_str):
    try:
        return float(size_str.replace('MB', ''))
    except ValueError:
        return pd.np.nan

# Apply the conversion to the 'size' column
merged_data['size'] = merged_data['size'].apply(convert_size_to_mb)

# Handle timeouts for scatterplot
merged_data['time'] = pd.to_numeric(merged_data['time'], errors='coerce')

# Combined Scatterplot for Time vs Size for both systems
plt.figure(figsize=(8, 6))
colors = ['blue', 'orange']  #  change these colors if needed
for i, system in enumerate(systems):
    system_data = merged_data[merged_data['system'] == system]
    plt.scatter(system_data['size'], system_data['time'], alpha=0.7, label=system, color=colors[i])

plt.title('Time vs Size for Each System')
plt.xlabel('Size (MB)')
plt.ylabel('Time')
plt.legend()
plt.grid(True)

# Save the plot in PDF format
plt.savefig('benchresults/time_vs_size_combined.pdf', format='pdf', bbox_inches='tight')

# Clear the current plot to avoid overlap with future plots
plt.clf()