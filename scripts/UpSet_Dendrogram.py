import pandas as pd
from upsetplot import UpSet 
import matplotlib.pyplot as plt 
import numpy as np 
from scipy.cluster.hierarchy import dendrogram, linkage
from scipy.spatial.distance import pdist, squareform


#Load CSV summary file 
csv_path = "full_output_summary.csv"
df = pd.read_csv(csv_path )

# Extract gene list from the 'Genes' column (separated by semicolons)
gene_sets = df['Genes'].apply(lambda x: set(str(x).split(";")))

# Build binary df indicating presence of each gene in each graph 
all_genes = set().union(*gene_sets)
binary_matrix = pd.DataFrame(
    [{gene: (gene in gs) for gene in all_genes} for gs in gene_sets], 
    index = [f"Graph_{i}" for i in range(len(gene_sets))]
)

# UpSet plot
upset = UpSet(binary_matrix, subset_size = "count", show_counts = "%d")
upset.plot()

plt.title("UpSet plot of shared genes across graphs")
plt.show()

# Create distance matrix and create dendrogram 

#Hierarchical clustering 
linkage_matrix = linkage(binary_matrix, method = "ward")

# Plot dendrogram
plt.figure(figsize=(14, 6))
dendrogram(linkage_matrix, labels=binary_matrix.index, leaf_rotation=90)
plt.title("Dendrogram of Graphs Based on Gene Set Similarity")
plt.ylabel("Distance")
plt.tight_layout()
plt.show()