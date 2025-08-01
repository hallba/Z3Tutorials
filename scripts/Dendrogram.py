import pandas as pd
from scipy.spatial.distance import squareform
from scipy.cluster.hierarchy import linkage, dendrogram
import matplotlib.pyplot as plt
import numpy as np

csv_files = [
    "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs.json_similarity_matrix.csv",
    "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs_HSC_overlap_mouseRNAseq_final_consistent.json_similarity_matrix.csv", 
    "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs_human_humanfinalConsistent.json_similarity_matrix.csv", 
    "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs_human_overlap_RAGEreceptors.json_similarity_matrix.csv"



]

def plot_dendrograms_from_csv(csv_paths):
    for csv_path in csv_paths:
        print(f"Processing {csv_path} ...")
        
        # Load CSV without header
        df_raw = pd.read_csv(csv_path, header=None)
        
        # Extract the first row as the "raw" headers (including the problematic first one)
        raw_headers = df_raw.iloc[0].tolist()
        
        # Shift headers left by one:
        # Discard the first header (upper-leftmost)
        # Then take from second header to the end, and append one empty string
        shifted_headers = raw_headers[1:] + [""]
        
        # Replace the first row in df_raw with shifted headers
        df_raw.iloc[0] = shifted_headers
        
        # Set shifted headers as column names and drop the first row
        df = df_raw.iloc[1:]
        df.columns = shifted_headers
        
        # Extract last column as row labels
        row_labels = df.iloc[:, -1]
        
        # Drop the last column from df (since it's row labels)
        df = df.iloc[:, :-1]
        
        # Set index to row labels
        df.index = row_labels
        
        # Convert all data to numeric 
        df = df.apply(pd.to_numeric, errors='coerce')
        
        # Convert to condensed form 
        condensed_dist = squareform(df)
        
        # Perform hierarchical clustering 
        Z = linkage(condensed_dist, method = "average")
        
        # Plot the dendrogram 
        plt.figure(figsize=(10, 6))
        dendrogram(
            Z,
            labels=df.index.tolist(),
            leaf_rotation=90
        )
        plt.title(f"Gene Similarity Dendrogram for {csv_path}")
        plt.tight_layout()
        plt.show()

plot_dendrograms_from_csv(csv_files)