import json 
import pandas as pd 
import matplotlib.pyplot as plt 

import json
import pandas as pd
import matplotlib.pyplot as plt

# Load graph data
with open("graphs.json", "r") as f:
    graphs = json.load(f)

# Count gene frequencies
from collections import Counter

gene_counter = Counter()

for graph in graphs:
    gene_names = set(var["Name"] for var in graph["Model"]["Variables"])
    for gene in gene_names:
        gene_counter[gene] += 1

print(len(graphs))

#Gene Frequency Histogram 
# Convert to DataFrame
df_freq = pd.DataFrame(gene_counter.items(), columns=["Gene", "Frequency"])
df_freq["Percent"] = 100 * df_freq["Frequency"] / len(graphs)

# Sort descending
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

core_genes = df_freq[df_freq["Frequency"] >= len(graphs) * 0.5]
print(f"Number of genes in ≥50% of graphs: {len(core_genes)}")

#Cumulative Frequency graph to explain 80% of gene usage
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
