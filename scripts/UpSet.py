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

# Plot Upset Graphs

df_1 = load_gene_presence_matrix("graphs.json")
plot_upset(df_1)

df_2 = load_gene_presence_matrix("graphs_human_overlap_RAGEreceptors.json")
plot_upset(df_2)

df_3 = load_gene_presence_matrix("graphs_HSC_overlap_mouseRNAseq_final_consistent.json")
plot_upset(df_3)

df_4 = load_gene_presence_matrix("graphs_human_humanfinalConsistent.json")
plot_upset(df_4)