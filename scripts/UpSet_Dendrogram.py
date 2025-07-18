import json
import pandas as pd
import matplotlib.pyplot as plt
from upsetplot import UpSet, from_indicators

# Load the JSON file (array of graphs)
with open("graphs.json", "r") as f:
    graphs = json.load(f)

# Initialize containers
network_gene_sets = []

# Loop through each graph
for graph in graphs:
    # Extract all gene names from "Model" -> "Variables" -> "Name"
    gene_names = set(var["Name"] for var in graph["Model"]["Variables"])
    network_gene_sets.append(gene_names)

# Get all unique genes across all graphs
all_genes = sorted(set.union(*network_gene_sets))

# Build presence/absence matrix (1 = present, 0 = absent)
matrix = []
for gene_set in network_gene_sets:
    row = [1 if gene in gene_set else 0 for gene in all_genes]
    matrix.append(row)

# Build DataFrame 
df = pd.DataFrame(matrix, columns=all_genes)

# Convert numeric 0/1 to boolean True/False
bool_df = df.astype(bool)

# Convert to UpSet-compatible format
upset_data = from_indicators(bool_df.columns, bool_df)

# Set smaller figure size
plt.figure(figsize=(10, 10))

# Create and plot UpSet plot
upset = UpSet(upset_data, subset_size="count", show_percentages=True)
upset.plot()
plt.show()