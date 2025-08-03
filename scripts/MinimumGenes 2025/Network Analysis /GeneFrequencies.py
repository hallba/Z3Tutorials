import json 
import pandas as pd 
import matplotlib.pyplot as plt 
from collections import Counter

# List of graph file paths 
graph_files = [ "graphs.json"
      "graphs_human_overlap_RAGEreceptors.json"
      "graphs_HSC_overlap_mouseRNAseq_final_consistent.json"
      "graphs_human_humanfinalConsistent.json" ]

# Count gene frequencies across all graphs 
gene_counter = Counter()
total_graphs = 0  # Keep track of total number of graphs

for file in graph_files:
    with open(file, "r") as f:
        graphs = json.load(f)
    
    total_graphs += len(graphs)
    
    for graph in graphs:
        gene_names = set(var["Name"] for var in graph["Model"]["Variables"])
        for gene in gene_names:
            gene_counter[gene] += 1

# Convert to Dataframe
df_freq = pd.DataFrame(gene_counter.items(), columns=["Gene", "Frequency"])
df_freq["Percent"] = 100 * df_freq["Frequency"] / total_graphs
df_freq = df_freq.sort_values(by="Frequency", ascending=False)

# Plot top N genes 
TOP_N = 16
plt.figure(figsize=(12, 6))
bars = plt.bar(df_freq["Gene"][:TOP_N], df_freq["Frequency"][:TOP_N], color="teal")
plt.xticks(rotation=90)
plt.ylabel("Number of Graphs")
plt.title(f"Top {TOP_N} Most Frequent Genes Across All Graphs")
plt.tight_layout()
plt.show()

# Core genes appearing in >= 50% of graphs 
core_genes = df_freq[df_freq["Frequency"] >= total_graphs * 0.5]
print(f"Number of genes in ≥50% of graphs: {len(core_genes)}")

# Cumulative coverage plot 
df_freq_sorted = df_freq.sort_values(by="Frequency", ascending=False)
df_freq_sorted["CumulativeCoverage"] = df_freq_sorted["Frequency"].cumsum() / df_freq_sorted["Frequency"].sum()

plt.figure(figsize=(10, 5))
plt.plot(range(len(df_freq_sorted)), df_freq_sorted["CumulativeCoverage"], marker='.')
plt.axhline(y=0.8, color='r', linestyle='--', label='80% coverage')
plt.xlabel("Top N Genes")
plt.ylabel("Cumulative Frequency Coverage")
plt.title("How many genes explain 80 percent of the network?")
plt.legend()
plt.grid()
plt.show()
