import json
import pandas as pd
import matplotlib.pyplot as plt
from upsetplot import UpSet, from_indicators

def load_gene_presence_matrix(json_path: str) -> pd.DataFrame: 
    """Load graphs from a JSON file and return a presence/absence DataFrame."""
    with open(json_path, "r") as f:
        graphs = json.load(f)

    network_gene_sets = []

    for graph in graphs:
        gene_names = set(var["Name"] for var in graph["Model"]["Variables"])
        network_gene_sets.append(gene_names)

    all_genes = sorted(set.union(*network_gene_sets))
    
    matrix = [
        [1 if gene in gene_set else 0 for gene in all_genes]
        for gene_set in network_gene_sets
    ]
    
    df = pd.DataFrame(matrix, columns=all_genes)
    return df.astype(bool)

def plot_upset(df: pd.DataFrame, figsize=(10, 10)):
    """Plot an UpSet diagram from a boolean presence/absence DataFrame."""
    upset_data = from_indicators(df.columns, df)
    plt.figure(figsize=figsize)
    upset = UpSet(upset_data, subset_size="count", show_percentages=True)
    upset.plot()
    plt.show()

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