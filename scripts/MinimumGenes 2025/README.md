Folder structure: 

MinimumGenes2025/  
│
├── Full Summary CSV/          # Outputs metadata about each generated network (config, gene counts, edge counts).  
├── Graphs JSON/               # Pretty-printed BMA-compatible JSON files for visualization.  
├── Network Analysis/          # Python scripts for plotting:  
│   ├── dendrogram.py          # Hierarchical clustering.  
│   ├── upset_diagram.py       # Gene set overlaps.  
│   └── frequency_plots.py     # Gene occurrence histograms.  
│
├── packages/                  # NuGet dependencies:  
│   ├── FSharp.Data.dll        # CSV/JSON parsing.  
│   └── Microsoft.Msagl.dll    # Graph layout engine.  
│
├── platform/                  # Z3 theorem prover binaries:  
│   ├── Microsoft.Z3.dll       # Core Z3 library.  
│   ├── Microsoft.Z3.xml       # Documentation.  
│   └── Microsoft.Z3.deps.json # Dependency metadata.  
│
├── Raw data CSV/              # Input gene sets (e.g., "human_overlap_RAGEreceptors.csv").  
├── Similarity Matrix/         # Jaccard similarity matrices (CSV) for clustering.  
│
├── z3/                        # Z3 binaries (platform-specific).  
├── z3-4.15.1-arm64-osx-13.7.6/  # macOS ARM64 Z3 build.  
│
├── getZ3.fsx                  # Z3 installation helper script.  
├── MinimumGenesUpdated.fsx    # Main analysis script.  
└── z3.zip                     # Compressed Z3 binaries (backup).   

## **User Configuration & Customization**

### **Key Customizable Parameters**

#### **1. `allOptions` (in MinimumGenesUpdated.fsx)**

    Purpose: Defines all possible configurations for gene network generation

    Default:

    let allOptions = [
        for oneDir in [true; false] do
        for strictFilter in [true; false] do
        for db in [PPI; Regulon; PTM; MiRNA; Pathways; Combo] do
        for source in [Human; Mouse; Rat] do
            yield { defaultInput with oneDirection = oneDir; strictFilter = strictFilter; database = db; source = source }
    ]

    Customization: Modify the lists to restrict options (e.g., remove Rat or MiRNA)

#### **2. `graphFiles` (in MinimumGenesUpdated.fsx)**

    Purpose: Lists JSON files for dendrogram analysis

    Default:

    let graphFiles = 
        [ "graphs.json"
          "graphs_human_overlap_RAGEreceptors.json"
          "graphs_HSC_overlap_mouseRNAseq_final_consistent.json"
          "graphs_human_humanfinalConsistent.json" ]

    Customization: Replace with your own output filenames

#### **3. `Timeout setting` (in MinimumGenesUpdated.fsx)**

    Purpose: Maximum runtime per network (milliseconds)

    Default: let timeoutMs = 1000000 (16.7 minutes)

    Recommendation:

        Reduce to 300000 (5 mins) for quick tests

        Increase for large gene sets (>50 genes)

#### **4. `CSV input/output names` (in MinimumGenesUpdated.fsx)**

    Input Genes:

    let humanRAGEReceptors = loadGeneArrayFromCsv "human_overlap_RAGEreceptors.csv"

    Output Files:
    fsharp

    writeGraphOutputCsvWithJsonReference "full_output_summary.csv" "graphs.json" outputs

#### **5. `csv_files` (in Dendrogram.py)**

    Purpose: Paths to similarity matrix CSV files for clustering  
    Default:   
        csv_files = [
            "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs.json_similarity_matrix.csv",
            "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs_HSC_overlap_mouseRNAseq_final_consistent.json_similarity_matrix.csv",
            "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs_human_humanfinalConsistent.json_similarity_matrix.csv",
            "/Users/lydialichen/internship25/Z3Tutorials/scripts/graphs_human_overlap_RAGEreceptors.json_similarity_matrix.csv"
        ]
#### **6. `graph_files` (in GeneFrequencies.py)**
    
    Purpose: List of BMA JSON graph files to analyse
    Default:   
        graph_files = [
        "graphs.json",
        "graphs_human_overlap_RAGEreceptors.json",
        "graphs_HSC_overlap_mouseRNAseq_final_consistent.json",
        "graphs_human_humanfinalConsistent.json"
    ]

#### **7. `TOP_N` (in GeneFrequencies.py)**

    Purpose: Number of top genes to show in frequency plots 
    Default: TOP_N = 16  
        