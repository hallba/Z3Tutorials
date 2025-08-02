(*
Three gene pairs, UV, WX,YZ
Each has the following shortest paths

U A B C V
U A B D V *
U E F G V

W A X

Y D Z
Y E Z

Want to find the smallest number of genes that link the original set
 U V W X Y Z

 *)

#r "nuget: FSharp.Data"
#r "/Users/lydialichen/internship25/Z3Tutorials/scripts/MinimumGenes 2025/platform/bin/Microsoft.Z3.dll"
#r "packages/Microsoft.Msagl.dll"
#r "System.IO.Compression.FileSystem.dll"

#load "getZ3.fsx"

open FSharp.Data
open System
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Diagnostics
open System.Threading.Tasks
open Microsoft.Z3 
open Microsoft.Msagl.Core.Geometry
open Microsoft.Msagl.Core.Geometry.Curves
open Microsoft.Msagl.Core.Layout
open Microsoft.Msagl.Core.Routing
open Microsoft.Msagl.Layout.Layered //For sugiyama
open Microsoft.Msagl.Layout.MDS
open Microsoft.Msagl.Layout.Incremental
open Microsoft.Msagl.Prototype.Ranking
open Microsoft.Msagl.Miscellaneous

(*
    These are URLs of Omnipath databases that have the same structure as the working example
    KinaseExtra (enzyme-substrate interactions)
    http://omnipathdb.org/interactions?datasets=kinaseextra&fields=sources&fields=references
    DoRothEA (TF-target interactions)
    http://omnipathdb.org/interactions?datasets=tfregulons&fields=sources&fields=references
    miRNA targets (miRNA-mRNA and TF-miRNA interactions)
    http://omnipathdb.org/interactions?datasets=mirnatarget&fields=sources,references
    Everything in the same format
    http://omnipathdb.org/interactions?datasets=omnipath,pathwayextra,kinaseextra,ligrecextra,tfregulons,mirnatarget&fields=sources,references
    Also of interest (but will not work here)
    Complexes
    http://omnipathdb.org/complexes?&fields=sources,references
*)
let interactionURL = "http://omnipathdb.org/interactions?fields=sources&fields=references"

(*
OmniPathData: 
    - Defines types (OmniPathData, OmniPathComplex, OmniSource, Organism) and function (getOmniData) 
    to load and filter interaction data from Omnipath API
    - Supports different datasets (OmniSource)
    - Uses CsvProvider for typed CSV data loading, to enable type-safe operations on data
*)

module OmniPathData =
    type OmniPath = CsvProvider<"http://omnipathdb.org/interactions?fields=sources&fields=references&&genesymbols=1", 
                                        Schema="Is_directed=bool,Consensus_inhibition=bool,Consensus_stimulation=bool">

    type OmniPathComplex = CsvProvider<"http://omnipathdb.org/complexes?&fields=sources,references">

    //let omni = OmniPath.Load(interactionURL+"&genesymbols=1")
    //let data = omni.Rows |> Seq.filter (fun x -> x.Is_directed && (x.Consensus_inhibition || x.Consensus_stimulation))

    //let omniComplex = OmniPathComplex.Load("http://omnipathdb.org/complexes?&fields=sources,references")
    //let complexData = omniComplex.Rows

    type OmniSource = PPI | Regulon | PTM | MiRNA | Pathways | Combo
    type Organism = Human | Mouse | Rat

    let getOmniData t strict o =  
            let oMod = match o with
                        | Human -> ""
                        | Mouse -> "&organisms=10090"
                        | Rat -> "&organisms=10116"
            let source = match t with 
                            | PPI -> "http://omnipathdb.org/interactions?fields=sources&fields=references&genesymbols=1" 
                            | PTM -> "http://omnipathdb.org/interactions?datasets=kinaseextra&fields=sources&fields=references&genesymbols=1"
                            | Regulon -> "http://omnipathdb.org/interactions?datasets=tfregulons&fields=sources&fields=references&genesymbols=1"
                            | MiRNA -> "http://omnipathdb.org/interactions?datasets=mirnatarget&fields=sources,references&genesymbols=1"
                            | Pathways -> "http://omnipathdb.org/interactions?datasets=pathwayextra&fields=sources,references&genesymbols=1"
                            | Combo -> "http://omnipathdb.org/interactions?datasets=omnipath,pathwayextra,kinaseextra,ligrecextra,tfregulons,mirnatarget&fields=sources,references&genesymbols=1"
                        |> fun x -> x + oMod 
            let data = OmniPath.Load(source)
            let filter = if strict then (fun (x: OmniPath.Row) -> x.Is_directed && (x.Consensus_inhibition || x.Consensus_stimulation)) else (fun x -> true)
            data.Rows |> Seq.filter filter

open OmniPathData

(*
Clipboard: 
    - Provides cross-platform clipboard support for Windows, macOS, Linux
    - Detects the OS and implements OS specific clipboard commands
*)

module Clipboard =
    type OS =
        | OSX        
        | Windows
        | Linux
    let getOS = 
            match (int Environment.OSVersion.Platform) with
            | (4 | 128) -> if File.Exists("/System/Library/CoreServices/SystemVersion.plist") then OSX else Linux
            | 6     -> OSX
            | _     -> Windows

    let macCopy (s: string) =
        let p = new Process()
        p.StartInfo <- new ProcessStartInfo("pbcopy", "-pboard general -Prefer txt")
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardOutput <- false
        p.StartInfo.RedirectStandardInput <- true
        p.Start() |> ignore
        p.StandardInput.Write(s)
        p.StandardInput.Close()
        p.WaitForExit()

    let winCopy (s: string) =
        File.WriteAllText ("tmp.json", s)
        let p = new Process()
        p.StartInfo <- new ProcessStartInfo("CMD.exe", "/C clip < tmp.json")
        p.StartInfo.UseShellExecute <- false
        p.Start() |> ignore
        p.WaitForExit()

    let sendToClipboard s = 
        match getOS with
        | Windows -> winCopy s
        | OSX -> macCopy s
        | Linux -> ()
open Clipboard

(*
GraphTypes: 
    - PartialPath: represents nodes in a path with parent linkage
    - Vertex: represents graph nodes with edges as OmniPath rows
    - InteractionType & InteractionInput describe types and details of interactions
*)

module GraphTypes = 
    type PartialPath = 
        {
            parent : PartialPath option
            name :string
        } with 
        member this.ToArray = 
            let rec core partial path = 
                match partial.parent with 
                | None -> (partial.name::path) |> Array.ofList
                | Some(n) -> core n (partial.name::path)
            core this []

        //Get the sign and references for the links
        member this.ToEdgeInfo =    
            let rec core partial path =
                    match partial.parent with
                    | None -> ()
                    | Some(n) -> ()
            core this []

    type Vertex = 
        {
            sname: string
            vertexId : int
            edges : OmniPath.Row list
        }

    type InteractionType = Activator | Inhibitor

    type InteractionInput = {
        source : string
        target : string
        description : string
        kind: InteractionType
        link: string
    }
open GraphTypes

(*
GraphBuilder: 
    - Converts OmniPath data into vertex structures indexed by gene symbol 
    - Builds dictionaries of edges and their interaction metadata from paths and data 
*)

module GraphBuilder = 
    let edgesToVertex data =
        let vertices = new Dictionary<string,OmniPath.Row list>()
        let dataArray = data |> Array.ofSeq
        Array.iter (fun (edge: OmniPath.Row) -> match vertices.TryGetValue edge.Source_genesymbol with
                                                | true, v -> vertices.[edge.Source_genesymbol] <- edge::v
                                                | _,_ -> vertices.Add(edge.Source_genesymbol,[edge])
            ) dataArray
        let vertexNames = vertices.Keys |> Array.ofSeq
        let numberSource = vertexNames |> Array.length
        let verticesArr = Array.init numberSource (fun i -> {edges = vertices.[vertexNames.[i]]; sname=vertexNames.[i]; vertexId=i} )
        let lookUp = new Dictionary<string,int>()
        Array.iteri (fun i name -> lookUp.Add(name,i)) vertexNames
        lookUp, verticesArr

    //Uses paths and the ominpath data to create a dictionary of connections
    let buildEdgeDictionary paths data maximal =
        let getEdges (lookUp: Dictionary<string,int>) (vertices: Vertex []) (d: Dictionary<string,InteractionInput>) path =
            let edgeNumber = Array.length path - 2
            for i=0 to edgeNumber do  
                let source = path.[i]
                let target = path.[i+1]
                let identifier = sprintf "%s>%s" source target
                match d.TryGetValue identifier, lookUp.TryGetValue source with 
                | (true, _), (_,_) -> ()
                | (_, _), (false,_) -> ()
                | _ -> 
                    //the next line will fail if the gene is not a source in the omnipath lookup
                    let edge = vertices.[lookUp.[source]].edges |> List.filter (fun x -> x.Target_genesymbol=target) 
                    match edge with 
                    | [] -> ()
                    | _ -> 
                        let omniEdge = edge |> List.last
                        let kind,sKind =  match omniEdge.Is_stimulation, omniEdge.Is_inhibition with
                                            | true, false -> "Activator", Activator
                                            | false, true -> "Inhibitor", Inhibitor
                                            | false, false -> "Unknown", Activator
                                            | _ -> "Mixed", Activator
                        let description = sprintf " %s %s-PMID:%s" source kind omniEdge.References
                        let inter = {
                                    source = source
                                    target=target
                                    kind=sKind
                                    description=description
                                    link=identifier
                                    }
                        d.Add(identifier, inter)
            
        let lookUp, vertices = edgesToVertex data
        let edgeDescriptions = new Dictionary<string,InteractionInput>()
        if maximal then 
            let allGenes = Array.concat paths |> Array.concat
            let allPairs = Array.collect (fun x -> Array.map (fun y -> [|x;y|]) allGenes) allGenes
            Array.iter (getEdges lookUp vertices edgeDescriptions) allPairs
        else 
            Array.iter (Array.iter (getEdges lookUp vertices edgeDescriptions)) paths
        edgeDescriptions

open GraphBuilder

(*
PathFinding: 
    - shortestPathLength: returns the length of the shortest path between source and target genes 
    - allShortestPaths: returns all shortest paths between source and target 
    - pairwisePathSearch: computes all shortest paths between every pair of genes in the array 
    - oneDirectionalHubSearch: computes shortest paths from one central "hub" gene to all other genes (paths where the hub is the source, and other genes are targets)
    - pairwiseHubSearch: computes both the downstream path from the source to target and upstream from target to source 
    - OneDirectionalPathSearch: for each gene pair, finds the shortest path in either direction

*)

module PathFinding = 

    let shortestPathLength source target data = 
        let lookUp, vertices = edgesToVertex data

        let getNextNodes visited source =
            match lookUp.TryGetValue source with 
            | true, n -> 
                let n = vertices.[n]
                List.map (fun (e:OmniPath.Row) -> e.Target_genesymbol ) n.edges
                |> List.filter (fun name -> not (Array.contains name visited))
                |> Array.ofList
            | _ -> Array.empty

        let rec core queue visited length =
            //For every item in the queue, find the next nodes and create a new queue from them
            let queue' = Array.collect (getNextNodes visited) queue 
            let success = Array.contains target queue'
            if success then
                Some(length)
            else if (Array.isEmpty queue') then
                None
            else
                let visited' = Array.concat [|visited; queue'|]
                core queue' visited' (length+1)
        core [|source|] [||] 1
        
    let allShortestPaths source target data =
        let lookUp, vertices = edgesToVertex data

        let getNextNodes source =
            match lookUp.TryGetValue source.name with 
            | true, n -> 
                let n = vertices.[n]
                List.map (fun (e:OmniPath.Row) -> {name=e.Target_genesymbol;parent=Some(source)} ) n.edges
                |> Array.ofList
            | _ -> [||]

        let rec core queue visited =
            //For every item in the queue, find the next nodes and create a new queue from them
            let queue' = Array.collect getNextNodes queue |> Array.filter (fun n -> not (Array.contains n.name visited) )
            let visited' = Array.concat [|visited; (Array.map (fun p -> p.name) queue')|]
            let success = Array.filter (fun x -> x.name = target) queue'
            if (Array.length success > 0) then
                success
            else if (Array.isEmpty queue') then
                [||]
            else
                core queue' visited'
        core [|{name=source;parent=None}|] [||]
        |> Array.map (fun p -> p.ToArray)

    let pairwisePathSearch data genes includeSelfLoops=
        Array.collect (fun geneI -> Array.map (fun geneJ -> if includeSelfLoops || geneI <> geneJ then allShortestPaths geneI geneJ data else [||]) genes ) genes
        |> Array.filter (fun pSet -> pSet<>[||])

    let oneDirectionalHubSearch hub data genes includeSelfLoops  =
        Array.map (fun gene -> allShortestPaths hub gene data) genes
        |> Array.filter (fun pSet -> pSet<>[||])

    let pairwiseHubSearch hub data genes includeSelfLoops  =
        let downstream = Array.map (fun gene -> allShortestPaths hub gene data) genes |> Array.filter (fun pSet -> pSet<>[||])
        let upstream = Array.map (fun gene -> allShortestPaths gene hub data) genes |> Array.filter (fun pSet -> pSet<>[||])
        Array.append downstream upstream

    let OneDirectionalPathSearch data genes includeSelfLoops =
        Array.mapi (fun i geneI -> 
            Array.mapi (fun j geneJ -> 
                if (i>=j) && (includeSelfLoops || geneI <> geneJ)
                then 
                    let ij = allShortestPaths geneI geneJ data 
                    let ji = allShortestPaths geneJ geneI data
                    if Array.isEmpty ij && Array.isEmpty ji then [||]
                    else if Array.isEmpty ij then ji
                    else if Array.isEmpty ji then ij
                    else if Array.length ij.[0] < Array.length ji.[0] then ij else ji
                else [||]) genes ) 
                genes
        |> Array.concat
        |> Array.filter (fun pSet -> pSet<>[||])

open PathFinding
    
(*
types: Defines types related to gene network modeling and input: variables, relationships, layout choices, 
input gene sources, main in put configurations
*)

module types =
    // Single node in the BMA tool format 
    type BmaVariable = {
                            id: int
                            name: string
                            x: float
                            y: float
                            formula: string
                            granularity: int
                            description: string
                        }
    
    //  Represents a directed edge between the BMA variables 
    type BmaRelationship = {
                                id: int
                                source: int
                                target: int
                                kind: string
                            }

    //  Specifies the graph layout algorithm to be used when displaying the network 
    type LayoutSelection = MDS | Sugiyama | Ranking | FastIncremental

    // Defines the source of raw data used for gene import 
    type DataSource = FileName of string | Data of string [] [] []

    // Specifies the source of input genes for a graph
    type GeneData =  Demo | FromArray of string [] | FromFile of string

    // Top-level config for generating a single gene interaction graph
    type MainInput = {
        genesSource : GeneData //Where to load genes from
        includeSelfLoops : Boolean // Whether to include self-loop edges
        oneDirection : Boolean // If true, paths are directional
        maximiseEdges : Boolean // If true, prioritise dense connections
        exclusions : string [] option // Optional list of genes to exclude
        database : OmniSource // Interaction database to use
        strictFilter : bool // Apply stricter constraints 
        hubGene : string option //all genes should be downstream of this if defined
        source: Organism // Organism source for filtering 
    }

    // Internal structure that holds all the data needed for building a graph
    type GraphInput = {
        ctx : Context
        m : Model
        zGenes : Sort
        paths : string [] [] []
        genes : string []
        inputGenes : string []
        layout: LayoutSelection
        interactionInfo : Dictionary<string,InteractionInput> option
        maxEdges : Boolean
        s : Optimize
        rotation: float
        numberGenesUsed: int
        Config: MainInput
    } 

    type CrossTalkInput = {
        genesSource1 : GeneData
        genesSource2 : GeneData
        includeSelfLoops : Boolean
        oneDirection : Boolean
        maximiseEdges : Boolean
        exclusions : string [] option
        database : OmniSource
        strictFilter : bool
        source : Organism
    }

    // Record Type storing information summarising each graph string output  
    type GraphSummary = {
            Graph: GraphInput
            GeneCount: int 
            InputGeneCoverage: int 
            EdgeCount: int
            Config: MainInput
        }


open types

(*
GeneIO:    
    - parsePath: parses a string of gene interaction paths, where each path consits of comma-separated genes
    - readFile: reads a file of path stirngs line by line, parses each line into a list of gene paths, and logs how many raw paths are reduced to distinct path blocks 
    - parseInteractions: parses a string describing a biological interaction between genes, to determine the direction and whether the activity is activating or inhibiting 
    - reads a file of gene interactions, parses each into a structured object, and returns a lookup dictionary indexed by the interaction string 
*)

module GeneIO =
    let parsePath (line:string) = 
        line.Split '#'
        |> fun x -> x.[..((Array.length x) - 2)]
        |> Array.map (fun (p:String) -> p.Split ',')

    let readFile name = 
        let mutable pathcount = 0
        let result = [|for line in File.ReadLines(name) do 
                        let paths = parsePath line
                        pathcount <- pathcount + (Array.length paths)
                        yield paths|]
        printf "Reducing %d possible paths to %d paths\n" pathcount (Array.length result)
        result

    let parseInteractions (line:string) = 
        let contents = line.Split '/'
        let interaction = contents.[0]
        let fullDescription = contents.[1]
        let kind = if (fullDescription.IndexOf("Inhibitor")<0) then Activator else Inhibitor
        let target = interaction.Split '>' |> fun x -> x.[1]
        let source = interaction.Split '>' |> fun x -> x.[0]
        {
            kind=kind
            target=target
            source=source
            description=fullDescription
            link=interaction
        }
    let readInteractionsFile name =
        let interactionType = new Dictionary<string,InteractionInput>()
        for line in File.ReadLines(name) do 
            let interaction = parseInteractions line
            interactionType.Add(interaction.link,interaction)
        interactionType
open GeneIO

(*
GeneUtils: Provides utility functions to work with genes, by interacting with the Z3 SMT solver context and model
    - countUsedGenes: iterates over all genes and checks in the Z3 model whether each gene is used. 
    - printGenes: prints out count of used genes
    - fancyInteractions: given an optional dictionary of interaction data and 2 genes (source, target), tries
    to retrieve the interaction details for that edge if it's available
    - localGeneUsed: for a given gene and a path ID, constructs a Z3 boolean variable representing 
    whether that gene is used in a specific path, by checking if the gene matches any variable in the path.
    - createVariables and pathToConstrai
    nt: generating Z3 variables representing paths and constraining these to match 
    specific gene sequences 
    - estimateComplexity: roughly estimates and prints the search space size (number of path alternatives) on a log scale, based on input gene data source
   
*)

module GeneUtils =
    // Counts how many genes are "used" (true in the model) and prints them 
    let countUsedGenes (ctx:Context) geneNames (m:Model)=
        let mutable used = 0
        let pairArray = Array.map (fun g-> (g,ctx.MkBoolConst(sprintf "Used-%s" g ))) geneNames
        for g,zg in pairArray do
            let state = sprintf "%O" (m.Eval(zg,true))
            if state = "true" then 
                printf "%s\n" g
                used <- used + 1
        used

    // Helper to print how many genes are used out of total
    let printGenes (ctx:Context) (m:Model) genes =
        let used = countUsedGenes ctx genes m
        printf "%d of %d genes used\n" used <| Array.length genes

    // Gets interaction info between 2 nodes if available 
    let fancyInteractions (data: Dictionary<string,InteractionInput> option) source target =
        match data with
        | None -> None
        | Some(iMap) -> 
            let key = sprintf "%s>%s" source target
            let information = iMap.[key]
            Some(information)
    //Has a gene been used in a path? 
    let localGeneUsed (ctx:Context) genes (pairMap:Map<string,Expr>) geneName pathID pathVars =
        let usedVar = ctx.MkBoolConst(sprintf "PathUsed-%d-%s" pathID geneName)
        let zGene = pairMap.[geneName]
        let used = Array.map (fun n -> ctx.MkEq(zGene,n)) pathVars |> fun x -> ctx.MkOr(x)
        (usedVar,ctx.MkEq(usedVar,used))
    //Has a gene been used in a path? 
    let geneUsed (ctx:Context) genes pathVars (pairMap:Map<string,Expr>) geneName =
        let usedVar = ctx.MkBoolConst(sprintf "Used-%s" geneName)
        let zGene = pairMap.[geneName]
        //let used = Array.map (fun n -> ctx.MkEq(zGene,n)) pathVars |> fun x -> ctx.MkOr(x)
        let lgu = localGeneUsed ctx genes pairMap geneName
        let used,ind =  Array.mapi lgu pathVars
                        |> fun r -> 
                            let vars = Array.map fst r
                            let individualPath = Array.map snd r
                            //If any of the path vars are true, then the global var is
                            (ctx.MkOr(vars),individualPath)
        (ctx.MkEq(usedVar,used),ind)
    let createVariables genes pathToConstraint pairs (ctx:Context) (altPaths: string [] []) = 
        //create a single set of variables that join the first and last elements
        let example = altPaths.[0]
        let namer =  
            let pathname = sprintf "%s-%s-%d" example.[0] (Array.last example) 
            fun i ->
                ctx.MkConst((pathname i), genes)
        //create an array of all variable names
        let variableNames = Array.mapi (fun i _ -> namer i) altPaths.[0]
        //specify that the paths should only be equal to the given paths
        Array.map (pathToConstraint pairs namer) altPaths |> fun x -> (variableNames,ctx.MkOr(x))
    let pathToConstraint (ctx:Context) (pairs: Map<string,Expr>) namer path  = 
        Array.mapi (fun i element -> ctx.MkEq((namer i),pairs.[element]) ) path
        |> fun x -> ctx.MkAnd(x)
    
    let estimateComplexity source = 
        let data = match source with 
                    | FileName(name) -> readFile name
                    | Data(res) -> res
        Array.sumBy (Array.length >> float >> Math.Log10) data
        |> printf "10^%f alternatives to be searched\n"
open GeneUtils

(*
GraphUtils: handles creating and visualising the graph from Z3 model results using MSAGL library
    - makeGraphInternal: creates a directed biological interaction graph using the Z3 model and layout settings from gI
        - makeNode: adds a circular node 
        - makeedge: adds a directed edge from source and target in the graph
        - getUsedNodes: checks if a gene should be included in the graph
        - bmaVariableModel: formats a node for the BMA "Model" section 
        - bmaVariableLayout: formats layout for the BMA "Layout" section 
        - bmaRelationshipText: formats an edge as a BMA "Relationship" with source and target IDs and kind: activator / inhibitor
        - idMapping: creates a mapping from node name to numeric ID, used in BMA JSON output, and later downstream for hierarchical clustering
        - edgeToBmaRel: evalutes source and target nod types and looks up interaction metadata
    - makeGraph: creates a directed biological interaction graph summary based on the provided input configuration and biological data, bundles information from GraphInput into a
    structured summary for downstream analysis 
        Inputs:
            - ctx: Z3 solver context for encoding SMT constraints 
            - m: Z3 model resulting from the solver, encoding the satisfiable configuration 
            - s: Z3 Optimise object used during solving, to extract objectives or constraints
            - zGenes: Z3 sort representing the gene type in the SMT model 
            - paths: A nested array of gene paths explored in the search 
            - genes: array of all genes involved in the final graph 
            - layoutAlgo: layout algorithm to be used for visualisation 
            - intData: optional dictioinary mapping gene interactions 
            - maxEdges: boolean flag to indicate if the graph was generated under the constraint of maximising edges
            - input: array of original input genes supplied by the user or dataset
            - used: number of input genes successfully used in the resulting graph 
            - config: full configuration used to generate the graph, for reproducibility 

        Outputs: Returns a "GraphSummary" record containing: 
            - "Graph": the full "GraphInput" data structure representing the graph 
            - "GeneCount": the number of nodes in the graph 
            - "InputGeneCoverage": the number of input genes successfully included in the graph 
            - "EdgeCount": number of edges / interactions present
            - "Config": the full configuration used to create the graph

*)
module GraphUtils = 
    let makeGraphInternal (gI:GraphInput) =
        let radius = 15.
        let makeNode (graph: GeometryGraph) radius name = 
            graph.Nodes.Add(Node(CurveFactory.CreateCircle(radius,Point()),name))
        let makeEdge (graph: GeometryGraph) source target = 
            graph.Edges.Add(Edge(graph.FindNodeByUserData(source),graph.FindNodeByUserData(target)))
        let getUsedNodes graph radius genes =
            let pairArray = Array.map (fun g-> (g,gI.ctx.MkBoolConst(sprintf "Used-%s" g ))) genes
            for g,zg in pairArray do
                let state = sprintf "%O" (gI.m.Eval(zg,true))
                if state = "true" then 
                    makeNode graph radius g
        let getUsedEdges graph (paths: string [] [] []) = 
            let foundEdges = new Dictionary<string*string,_>()
            let getEdgesPath (individualPath: string [] []) =
                let example = individualPath.[0]
                let namer =  
                    let pathname = sprintf "%s-%s-%d" example.[0] (Array.last example) 
                    fun i ->
                        gI.ctx.MkConst((pathname i), gI.zGenes)
                //create an array of all variable names
                let variableNames = Array.mapi (fun i _ -> namer i) individualPath.[0]
                let edgeNumber = (Array.length variableNames) - 2
                for i = 0 to edgeNumber do
                    let source = sprintf "%O" (gI.m.Eval(variableNames.[i],true))
                    let target = sprintf "%O" (gI.m.Eval(variableNames.[i+1],true))
                    match foundEdges.TryGetValue((source,target)) with
                    | false,_ ->    foundEdges.Add((source,target),true)
                                    makeEdge graph source target
                    | _,_ -> ()
            if gI.maxEdges then
                //We need the keys of the edge annotation dictionary, the genes that are present. 
                //Filter by the genes present, and then add everything else
                let pairArray = Array.map (fun g-> (g,gI.ctx.MkBoolConst(sprintf "Used-%s" g ))) gI.genes
                let mutable used = []
                for g,zg in pairArray do
                    let state = sprintf "%O" (gI.m.Eval(zg,true))
                    if state = "true" then 
                        used <- g::used
                let allEdges = gI.interactionInfo.Value.Keys |> Array.ofSeq
                let edges = List.collect (fun i -> List.map (fun j -> i,j) used ) used
                            |> List.filter (fun x -> Array.contains (sprintf "%s>%s" (fst(x)) (snd(x))) allEdges)
                for source,target in edges do
                    foundEdges.Add((source,target),true)
                    makeEdge graph source target

            else
                Array.iter getEdgesPath paths 
        let graph = GeometryGraph()
        //Add nodes
        //makeNode graph radius "A"
        //makeNode graph radius "B"
        getUsedNodes graph radius gI.genes
        //Add edges
        //makeEdge graph "A" "B"
        getUsedEdges graph gI.paths
        //Parameters for layout
        let routingSettings = EdgeRoutingSettings(EdgeRoutingMode = EdgeRoutingMode.StraightLine)

        match gI.layout with
        | MDS      ->           let settings = MdsLayoutSettings(EdgeRoutingSettings = routingSettings)
                                //let layout = MdsGraphLayout(settings,graph)
                                //layout.Run()

                                LayoutHelpers.CalculateLayout(graph,settings,null)
        | Sugiyama ->           let settings =  SugiyamaLayoutSettings(EdgeRoutingSettings = routingSettings)
                                //let layout =  LayeredLayout(graph,settings)
                                //layout.Run()
                                LayoutHelpers.CalculateLayout(graph,settings,null)
        | Ranking ->            let settings = RankingLayoutSettings(EdgeRoutingSettings = routingSettings)
                                //let layout = RankingLayout(settings,graph)
                                //layout.Run()
                                LayoutHelpers.CalculateLayout(graph,settings,null)
        | FastIncremental ->    let settings = FastIncrementalLayoutSettings(EdgeRoutingSettings = routingSettings)
                                LayoutHelpers.CalculateLayout(graph,settings,null)
        | _ -> failwith "Not implemented"

        //Now format a model with all of the interactions

        let bmaVariableModel bmaVar =
            sprintf "{\"Name\":\"%s\",\"Id\":%d,\"RangeFrom\":0,\"RangeTo\":%d,\"Formula\":\"%s\"}" bmaVar.name bmaVar.id bmaVar.granularity bmaVar.formula 
        let bmaVariableLayout angle (bmaVar: BmaVariable) =
            let x' = bmaVar.x*cos(angle) - bmaVar.y*sin(angle)
            let y' = bmaVar.x*sin(angle) + bmaVar.y*cos(angle)
            if Array.contains bmaVar.name gI.inputGenes then
                sprintf "{\"Id\":%d,\"Name\":\"%s\",\"Type\":\"Constant\",\"ContainerId\":0,\"PositionX\":%f,\"PositionY\":%f,\"CellX\":0,\"CellY\":0,\"Angle\":0,\"Description\":\"%s\",\"Fill\": \"BMA_Green\"}" bmaVar.id bmaVar.name x' y' bmaVar.description
            else 
                sprintf "{\"Id\":%d,\"Name\":\"%s\",\"Type\":\"Constant\",\"ContainerId\":0,\"PositionX\":%f,\"PositionY\":%f,\"CellX\":0,\"CellY\":0,\"Angle\":0,\"Description\":\"%s\"}" bmaVar.id bmaVar.name x' y' bmaVar.description
        let bmaRelationshipText bmaRel =
            sprintf "{\"Id\":%d,\"FromVariable\":%d,\"ToVariable\":%d,\"Type\":\"%s\"}" bmaRel.id bmaRel.source bmaRel.target bmaRel.kind
        let idMapping (n: Node IList) =
            let nameToID = new Dictionary<string,int>()
            for i=0 to (n.Count-1) do
                nameToID.Add(n.[i].UserData.ToString(),i)
            nameToID
        
        let nodeAnnotation = new Dictionary<string,string>()
        let nodeToBmaVar (n:Node) i = 
            let name = n.UserData.ToString()
            {   name = name
                x = n.Center.X
                y = n.Center.Y
                id = i
                formula = ""
                granularity = 2
                description = 
                match nodeAnnotation.TryGetValue name with
                            | true,d -> d
                            | _,_ -> ""
            }
        let edgeToBmaRel (e:Edge) (nameToID:Dictionary<string,int>) i =
            let source = e.Source.UserData.ToString()
            let target = e.Target.UserData.ToString()
            let kind =  match (fancyInteractions gI.interactionInfo source target) with
                        | None -> "Activator"
                        | Some(i) -> //Store node description data
                                    let d = i.description
                                    match nodeAnnotation.TryGetValue target with
                                    | true, v -> nodeAnnotation.[target] <- v+d
                                    | false,_ -> nodeAnnotation.Add(target,d)
                                    //Extract the node information
                                    match i.kind with 
                                    | Activator -> "Activator"
                                    | _ -> "Inhibitor"
            {
                kind = kind
                id = i
                source = nameToID.[source]
                target = nameToID.[target]
            }
        //Sugiyama looks great but goes up/down making gene names hard to read, so rotate if thats the case
        //let rotation = if gI.layout = Sugiyama then Math.PI/2. else 0.

        //convert nodes into appropriate strings
        let nodeCount = graph.Nodes.Count
        let nameToID = idMapping graph.Nodes
        //find the interactions first, to build up the description dictionary, then build the variables
        let interactions =  Array.ofSeq graph.Edges 
                            |> Array.mapi (fun i e -> edgeToBmaRel e nameToID (i+nodeCount) |> bmaRelationshipText) 
                            |> fun x -> String.Join(",",x)
        let bmaVariables = Array.init nodeCount (fun i -> nodeToBmaVar graph.Nodes.[i] i )
        let varModel = Array.map bmaVariableModel bmaVariables |> fun x -> String.Join(",",x) 
        let varLayout = Array.map (bmaVariableLayout gI.rotation) bmaVariables |> fun x -> String.Join(",",x) 

        let result = sprintf "{\"Model\": {\"Name\": \"Omnipath motif\",\"Variables\":[%s],\"Relationships\":[%s]},\"Layout\":{\"Variables\":[%s],\"Containers\":[]}}\n" varModel interactions varLayout
        result

    // Record type to hold all makeGraph input parameters 

    type MakeGraphInput = {
        ctx: Context
        m: Model
        s: Optimize
        zGenes: Sort
        paths: string [][][]
        genes: string []
        layoutAlgo: LayoutSelection
        intData: Dictionary<string,InteractionInput> option
        maxEdges: bool
        input: string []
        used: int
        config: MainInput
    }

    let makeGraph (input:MakeGraphInput): GraphSummary =
        let (gI:GraphInput) = {
                ctx = input.ctx
                m = input.m
                zGenes = input.zGenes
                paths = input.paths
                genes = input.genes
                layout = input.layoutAlgo
                interactionInfo = input.intData
                maxEdges = input.maxEdges
                s = input.s
                rotation = 0.
                inputGenes = input.input
                numberGenesUsed = input.used
                Config = input.config
        }

        // Count number of genes
        let geneCount = input.genes.Length

        // Count number of edges / interactions 
        let edgeCount = 
            match input.intData with 
            | Some dict -> dict.Count
            | None -> 0
            
        // Count how many input genes appear in the graph 
        let inputGeneCoverage = 
            input.input |> Array.filter (fun gene -> Array.contains gene input.genes) |> Array.length

        // Return the summary
        {
            Graph = gI
            GeneCount = geneCount 
            InputGeneCoverage = inputGeneCoverage
            EdgeCount = edgeCount
            Config = input.config
        }
open GraphUtils

(*
PathSearch: Retrieves paths for different gene sets and data soures 
    - getPathsFromSource: returns sets of paths 
        From Demo source: returns 3 predefined path groups corresponding to example genes and their paths
        From FromArray / FromFile sources: reads the gene sets and calls the provided search function with the data

*)
module PathSearch =
    let getPathsFromSource source data search selfLoops = 
        match source with
        | Demo ->   [|
                          [|
                              [|"U";"A";"B";"C";"V"|]
                              [|"U";"A";"B";"D";"V"|]
                              [|"U";"E";"F";"G";"V"|]
                          |];
                          [|
                              [|"W";"A";"X"|]
                          |];
                          [|
                              [|"Y";"E";"Z";|]
                              [|"Y";"D";"Z";|]
                          |];
                    |]
        | FromArray(arr) -> search data arr selfLoops
        | FromFile(name) -> 
                            let arr = [| for line in File.ReadLines(name) do 
                                            yield line |]
                            search data arr selfLoops
open PathSearch

(*
MainSolver: overall logic for setting up, running and analysing the gene pathway optimisation problems using 
the Z3 SMT solve, supporting both single gene set analysis and crosstalk analysis between 2 gene sets. 
    
    defaultInput: defines default parameters for a typical run
    main: 
        1. choose the search method 
        2. calls getOmniData to retrieve interaction data from specified database, with strict filtering 
        and source species, also applying exclusions if they are provided
        3. getting the paths: retrieve possible gene paths from the gene source and filtered data. 
        4. handle no paths found by returning None
        5. estimate complexity 
        6. set up a Z3 context and variables: creates an SMT optimisation solver and creates a Z3 context
        7. create Z3 variables and constraints for each path. Determine if a gene is used, and add gene usage constraints to the solver.
        8. count how many genes are used and add this constraint, setting the optimisation goal to minimise this number 
        9. solve and return the graph data if satisfiable, otherwise, return none / throw an error
    defaultCrossTalkInput: defines default parameters for cross-talk analysis between 2 gene sets and encourage more connectivity
    crossTalk: 
        1, 2. similar to defaultInput
        3. getting the paths for both gene sets
        4. checks if there are any overlapping genes between the 2 gene sets: 
            if yes: 
                - combine gene sets and paths and build an interaction dictionary -> 6.7 in main 
                - adds a constaint enforcing that at least one of the overlapping genes must be used 
                - 8, 9. in main 
            if no: 
                - finds the shortest path length between any gene in the first set to any gene in the second set
*)
module MainSolver = 

    let defaultInput = 
        {
            genesSource = Demo
            includeSelfLoops = false
            oneDirection = true
            maximiseEdges = false
            exclusions = None
            database = PPI
            strictFilter = true
            hubGene = None
            source = Human
        }
    

    let main input =
        let search =    match input.hubGene,input.oneDirection with 
                        | None,true -> OneDirectionalPathSearch
                        | None,_ -> pairwisePathSearch
                        | Some(hub),true -> oneDirectionalHubSearch hub
                        | Some(hub),_ -> pairwiseHubSearch hub

        let db = getOmniData input.database input.strictFilter input.source
        let data =  match input.exclusions with
                            | None -> db
                            | Some(x) -> Seq.filter (fun (row:OmniPath.Row) -> Array.contains row.Source_genesymbol x || Array.contains row.Target_genesymbol x |> not ) db

        
        let (paths: string [] [] []) = getPathsFromSource input.genesSource data search input.includeSelfLoops 
        let interactions =  match input.genesSource with 
                            | Demo -> None
                            | _ -> Some(buildEdgeDictionary paths data input.maximiseEdges)
        
        if Array.sumBy Array.length paths = 0 then printf "No paths found between genes\n"; None else
            estimateComplexity <| Data(paths)
            let geneNames = Array.collect Array.concat paths |> Array.distinct

            let ctx = new Context()
            //This could be done with bitvectors and a manual numbering scheme but enumsort are converted anyway..
            let genes = ctx.MkEnumSort("genes",geneNames)
            let pairs = Array.mapi (fun i g -> (g,genes.Consts.[i] )) geneNames |> Map.ofArray

            let s = ctx.MkOptimize()
            //let g = ctx.MkGoal()
            let varnames,vars = Array.map (createVariables genes (pathToConstraint ctx) pairs ctx) paths 
                                |> fun x -> 
                                    let names = Array.map fst x 
                                    let behaviour = Array.map snd x
                                    names,behaviour

            Array.map (geneUsed ctx genes varnames pairs) geneNames
            |> Array.iter (fun (g,l) -> s.Add(l); s.Add(g))

            let geneNumber = ctx.MkIntConst("GeneCount")
            let boolToNumber (ctx: Context) n =
                ctx.MkITE(ctx.MkBoolConst(sprintf "Used-%s" n), ctx.MkInt(1),ctx.MkInt(0)) :?> ArithExpr
            let countGenes = Array.map (fun n -> boolToNumber ctx n) geneNames  |> fun x -> ctx.MkAdd(x)
            let measure = ctx.MkEq(geneNumber,countGenes)

            s.Add(vars)   
            //s.Add(countingElements) 
            s.Add(measure)
            ignore <| s.MkMinimize(geneNumber)
            //printf "%s\n" <| s.ToString()
            match s.Check() with
                | Status.SATISFIABLE -> 
                    printf "sat\n"
                    //printf "%s\n" <| s.Model.ToString()
                    printGenes ctx s.Model geneNames 
                    
                    let inputGenes = 
                        match input.genesSource with
                                    | Demo -> [||]
                                    | FromArray(a) -> a
                                    | FromFile(name) -> 
                                    [| for line in File.ReadLines(name) do 
                                                            yield line |]
                    
                    let used = countUsedGenes ctx geneNames s.Model

                    let inputRecord : MakeGraphInput = {
                        ctx = ctx
                        m = s.Model
                        s = s
                        zGenes = genes
                        paths = paths
                        genes = geneNames
                        layoutAlgo = Sugiyama
                        intData = interactions
                        maxEdges = input.maximiseEdges
                        input = inputGenes
                        used = used
                        config = input
                    }
                    
                    let result = makeGraph inputRecord

                    //Return both inputs for makeGraphInternal to enable replotting
                    Some(result)
                | Status.UNSATISFIABLE -> 
                    printf "unsat"
                    None
                | _ -> failwith "unknown"
    let defaultCrossTalkInput = 
        {
            genesSource1 = Demo
            includeSelfLoops = false
            oneDirection = true
            maximiseEdges = true
            exclusions = None
            genesSource2 = Demo
            database = PPI
            strictFilter = true
            source = Human
        }
    let crossTalk input = 
        //Calculates the minimum distance between two sets of genes
        //Then calculates the mininum set of genes for each pathway, and required to connect the two pathways
        let search = if input.oneDirection then OneDirectionalPathSearch else pairwisePathSearch
        let db = getOmniData input.database input.strictFilter input.source
        let data =  match input.exclusions with
                            | None -> db
                            | Some(x) -> Seq.filter (fun (row:OmniPath.Row) -> Array.contains row.Source_genesymbol x || Array.contains row.Target_genesymbol x |> not ) db

        let data =  match input.exclusions with
                            | None -> data
                            | Some(x) -> Seq.filter (fun (row:OmniPath.Row) -> Array.contains row.Source_genesymbol x || Array.contains row.Target_genesymbol x |> not ) data
        let paths1 = getPathsFromSource input.genesSource1 data search input.includeSelfLoops
        let paths2 = getPathsFromSource input.genesSource2 data search input.includeSelfLoops

        //Do the paths overlap? If so, continue as normal enforcing at least one connection

        let geneNames1 = Array.collect Array.concat paths1 |> Array.distinct
        let geneNames2 = Array.collect Array.concat paths2 |> Array.distinct

        let overlap = Array.fold (fun acc gene -> if (Array.contains gene geneNames2) then gene::acc else acc ) [] geneNames1

        match overlap with
        | _::_ -> 
            let geneNames = Array.append geneNames1 geneNames2 |> Array.distinct
            let paths = Array.append paths1 paths2
            
            let ctx = new Context()
            let interactions =  match input.genesSource1 with 
                                | Demo -> None
                                | _ -> Some(buildEdgeDictionary paths data input.maximiseEdges)
            estimateComplexity <| Data(paths)
            //This could be done with bitvectors and a manual numbering scheme but enumsort are converted anyway..
            let genes = ctx.MkEnumSort("genes",geneNames)
            let pairs = Array.mapi (fun i g -> (g,genes.Consts.[i] )) geneNames |> Map.ofArray

            let s = ctx.MkOptimize()
            //let g = ctx.MkGoal()
            let varnames,vars = Array.map (createVariables genes (pathToConstraint ctx) pairs ctx) paths 
                                |> fun x -> 
                                    let names = Array.map fst x 
                                    let behaviour = Array.map snd x
                                    names,behaviour
            Array.map (geneUsed ctx genes varnames pairs) geneNames
            |> Array.iter (fun (g,l) -> s.Add(l); s.Add(g))

            let geneNumber = ctx.MkIntConst("GeneCount")
            let boolToNumber (ctx: Context) n =
                ctx.MkITE(ctx.MkBoolConst(sprintf "Used-%s" n), ctx.MkInt(1),ctx.MkInt(0)) :?> ArithExpr
            let countGenes = Array.map (fun n -> boolToNumber ctx n) geneNames  |> fun x -> ctx.MkAdd(x)
            let measure = ctx.MkEq(geneNumber,countGenes)

            //Need to specify that at least one overlapping gene is enforced
            s.Add(ctx.MkOr(List.map (fun n -> ctx.MkBoolConst(sprintf "Used-%s" n)) overlap))

            s.Add(vars)   
            //s.Add(countingElements) 
            s.Add(measure)
            ignore <| s.MkMinimize(geneNumber)
            //printf "%s\n" <| s.ToString()
            match s.Check() with
                | Status.SATISFIABLE -> 
                    printf "sat\n"
                    //printf "%s\n" <| s.Model.ToString()
                    printGenes ctx s.Model geneNames

                    // Build inputGenes by combining genesSource1 and genesSource2
                    let inputGenes =
                        let extractGenes source =
                            match source with
                            | Demo -> [||]
                            | FromArray a -> a
                            | FromFile name -> [| for line in File.ReadLines(name) -> line |]
                        Array.append (extractGenes input.genesSource1) (extractGenes input.genesSource2) |> Array.distinct
                    
                    // Count how many input genes are used in the result
                    let used = countUsedGenes ctx geneNames s.Model

                    // Build a config (MainInput) record from CrossTalkInput
                    let input = {
                        database = input.database
                        source = input.source
                        strictFilter = input.strictFilter
                        exclusions = input.exclusions
                        genesSource = input.genesSource1
                        maximiseEdges = input.maximiseEdges
                        includeSelfLoops = input.includeSelfLoops
                        oneDirection = input.oneDirection
                        hubGene = None
                    }

                    // Construct the full input record for makeGraph
                    let inputRecord : MakeGraphInput = {
                        ctx = ctx
                        m = s.Model
                        s = s
                        zGenes = genes
                        paths = paths
                        genes = geneNames
                        layoutAlgo = Sugiyama
                        intData = interactions
                        maxEdges = input.maximiseEdges
                        input = inputGenes
                        used = used
                        config = input
                    } 

                    // Generate the output visualization data
                    let result = makeGraph inputRecord
                    
                    //Return both inputs for makeGraphInternal to enable replotting
                    Some result

                | Status.UNSATISFIABLE -> 
                    printf "unsat"
                    None
                | _ -> failwith "unknown"
            
        | _ -> 
            //Sets of genes do not overlap. 
            //Need to find the shortest paths between any genes in the sets and enforce one
            // (or two for bidirectional) of those short links
            let mutable shortest = None
            for source in geneNames1 do
                for target in geneNames2 do
                    match shortestPathLength source target data with
                    | None -> ()
                    | Some(l) -> match shortest with
                                    | None -> shortest <- Some(source,target,l)
                                    | Some(_,_,lOrig) -> if lOrig <= l then () else shortest <- Some(source,target,l)

            failwith "Incomplete"
open MainSolver

(*
GeneGraph: Defines the input gene set and systematically generated a list of all possible configuration options for graph construction
    Notes: The current total number of configurations is 2 (oneDirection) x 2 (strictFilter) x 6 (database) x 3 (source) = 72 
    If the commented options are included, the full space reaches 288
*)
module GeneGraph =
    let mouseGenesMonika = [| "Agps";"Coro7";"Epdr1";"Fth1";"Ftl1";"Mocs3";"Rap2b";"Serpina1a";"Sh3bp1";"Slc14a1";"Tgm2";"Upp1"|]

    // All combinations of input settings (288 in total)
    let allOptions =
        [
            //for selfLoops in [true; false] do
            for oneDir in [true; false] do
            //for maxEdges in [true; false] do
            for strictFilter in [true; false] do
            for db in [PPI; Regulon; PTM; MiRNA; Pathways; Combo] do
            for source in [Human; Mouse; Rat] do
                yield {
                    defaultInput with
                        includeSelfLoops = false
                        oneDirection = oneDir
                        maximiseEdges = false
                        strictFilter = strictFilter
                        database = db
                        source = source
                }
        ]
open GeneGraph

(*
AsyncHelper: a helper function that runs an asynchronous workflow with a timeout
*)

module AsyncHelper = 
    // Helper function: Adds timeout 
    let asyncWithTimeout (timeoutMs: int) (workflow: Async<'T>) : Async<'T option> = async {
        let task = Async.StartAsTask workflow
        let timeoutTask = Task.Delay(timeoutMs)
        let! completed = Task.WhenAny(task, timeoutTask) |> Async.AwaitTask
        if completed = (task :> Task) then
            let! result = workflow
            return Some result
        else
            return None
    }
open AsyncHelper

(*
Compression: Compresses a UTF-8 encoded input string using GZip, then encodes it as a Base64 string
*)

module Compression = 
    // Compression function
    let compressToBase64 (input:string): string = 
            let bytes = Encoding.UTF8.GetBytes(input)
            use output = new MemoryStream()
            use gzip = new GZipStream(output, CompressionMode.Compress)
            gzip.Write(bytes, 0, bytes.Length)
            gzip.Close()
            Convert.ToBase64String(output.ToArray())
open Compression 

(*
GraphRunner: runs the "main" graph generation function across all predefined configuration options , injecting 
a shared set of input genes into each run. Applies a timeout to each run to avoid blocking indefinetely on
unsatisfiable or long-running configurations, and collects all successful outputs 
*)

module GraphRunner =
    
    // Represents the output of a graph generation run where "Graph": the BMA-compatible graph string, Base-64 compressed, and "Summary": metadata 
    type GraphOutput = {
        Graph: string // BMA-compatible string 
        Summary:GraphSummary // Full config + gene info for CSV export
    }

    // Runs the "main" function across all graph configuraitons using a shared input gene set
    // applied a timeout to each run, and collect all successful results 

    let runAllWithGenes (genes: string[]) = async {
        // Collect all graph outputs that are successfully computed
        let allOutputs = new List<GraphOutput>()
        let timeoutMs = 1000000 // Set timeout per config (in milliseconds)

        // Iterate through all configurations
        for i, (conf: MainInput) in allOptions |> List.mapi (fun i c -> (i, c)) do
            // Inject current gene list into config
            let configWithGenes = { conf with genesSource = FromArray genes }

            printfn "[%d/%d] Trying config: %A" (i + 1) allOptions.Length configWithGenes

            // Wrap 'main' call in a Task to make asynchronous, and apply timeout
            // assign the result of the async operation (main runner function with configWithGenes)to resultOpt
            let! resultOpt =
                asyncWithTimeout timeoutMs (async {
                    // Run main on thread pool since it's blocking
                    return! Async.AwaitTask(Task.Run(fun () -> main configWithGenes))
                })

            match resultOpt with
            | Some (Some summary) ->
                // Successfully got a graph summary
                let rawJson = makeGraphInternal summary.Graph
                let compressedBMA = compressToBase64 rawJson
                let output = { Graph = compressedBMA; Summary = summary}
                printfn "Graph generated for config %d" (i + 1)
                allOutputs.Add(output)

            | Some None ->
                // main ran successfully but returned None (e.g. unsat)
                printfn "No graph found for this configuration."

            | None ->
                // Timeout occurred
                printfn "Timeout after %d ms for config %d, skipping." timeoutMs (i + 1)

        // Convert list to immutable format
        let outputs = allOutputs |> Seq.toList

        // Display graph summary
        if outputs.Length = 0 then
            printfn "No graphs generated."
        else
            printfn "\nGenerated %d graphs. Select one by index (0 to %d):" outputs.Length (outputs.Length - 1)
            outputs
            |> List.iteri (fun i o ->
                let preview =
                    if o.Graph.Length > 100 then o.Graph.Substring(0, 100) + "..."
                    else o.Graph
                printfn "[%d]: %s" i preview)

        return outputs
    }
open GraphRunner

(*
IOSummary: writes graph outputs and their metadata to both a structured CSV file and a full JSON file 
*)

module IOSummary = 
    
    (* Writes a CSV summarising a list of GraphOutput data
    The BMAString is included raw so the CSV opens cleanly in Excel and the BMAStirng can be directly copy-pasted into the BMA web app*)

    /// Write all full raw JSON BMA strings to a pretty-printed JSON file
    let writeBmaJsonFile (filename: string) (outputs: GraphOutput list) =
        use writer = new StreamWriter(filename)
        
        // Create empty JsonArray 
        let jsonArray = JsonArray()
        // Parse and add each JSON node to the array
        for o in outputs do
            let rawJson = 
                if o.Graph.StartsWith("H4sI") then
                    // decompress base64 gzip to raw JSON string
                    let decompress (base64Str: string) =
                        let compressedBytes = Convert.FromBase64String(base64Str)
                        use inputStream = new MemoryStream(compressedBytes)
                        use gzipStream = new IO.Compression.GZipStream(inputStream, IO.Compression.CompressionMode.Decompress)
                        use reader = new StreamReader(gzipStream)
                        reader.ReadToEnd()
                    decompress o.Graph
                else
                    o.Graph
            let jsonNode = JsonNode.Parse(rawJson)
            jsonArray.Add(jsonNode)
            
        // Serialize entire array with indentation
        let options = JsonSerializerOptions(WriteIndented = true)
        let jsonString = jsonArray.ToJsonString(options)

        // Write the full JSON array to the file
        writer.Write(jsonString)
    
    /// Write CSV summary referencing the JSON file via an index column
    let writeGraphOutputCsvWithJsonReference (filenameCsv: string) (filenameJson: string) (outputs: GraphOutput list) = 
        // First write the JSON file
        writeBmaJsonFile filenameJson outputs

        let sb = StringBuilder()
        sb.AppendLine "Database,Source,Selfloops,OneDirection,MaximiseEdges,StrictFilter,GeneCount,InputGeneCoverage,EdgeCount,Genes,JsonIndex" |> ignore

        for i, o in outputs |> List.mapi (fun i o -> i, o) do
            let s = o.Summary
            let cfg = s.Config

            let geneList = 
                match cfg.genesSource with 
                | FromArray arr -> arr |> String.concat ";"
                | FromFile path -> path 
                | Demo -> "Demo"

            sb.AppendLine(sprintf "%A,%A,%b,%b,%b,%b,%d,%d,%d,\"%s\",%d"
                cfg.database cfg.source cfg.includeSelfLoops cfg.oneDirection
                cfg.maximiseEdges cfg.strictFilter
                s.GeneCount s.InputGeneCoverage s.EdgeCount
                geneList
                i) |> ignore

        File.WriteAllText(filenameCsv, sb.ToString())

    /// Loads a list of genes from the first column of the CSV files
    let loadGeneArrayFromCsv (path: string) =
            File.ReadLines(path)                            // Lazily read all lines from the CSV file
            |> Seq.skip 1                                   // Skip the header row (usually the first line)
            |> Seq.choose (fun line ->
                // Determine the delimiter based on the line content (semicolon or comma)
                let cols = 
                    if line.Contains(";") then line.Split(';')
                    else line.Split(',')

                if cols.Length > 1 then
                    // Take the second column (index 1), trim whitespace, and remove any surrounding quotes
                    let gene = cols.[1].Trim().Replace("\"", "")
                    // Only include non-empty, non-whitespace entries
                    if not (String.IsNullOrWhiteSpace(gene)) then
                        Some gene
                    else None
                else None)                                  // Skip lines that don’t have at least two columns
            |> Seq.toArray                                  // Convert the final filtered sequence into an array
open IOSummary 

(*
Z3optimisedDendrogram: contains records in the correct format of the JSON pretty-printed files generated 
*)
module Z3optimisedDendogram = 
    
    // Mapping of a gene to the set of edges it participates in within a specific graph 
    type GeneEdgeMapping = {
        GraphId: int 
        Gene:string 
        Edges: Set<string * string> // (source, target)
    }

    // Variable (node) in a BMA model 
    type BmaVariable = {
        Id: int
        Name: string
    }

    // Directed relationship between 2 variables in the BMA model 
    type BmaRelationship = {
        Id: int
        FromVariable: int
        ToVariable: int
        kind: string
    }

    // Core model structure of a BMA graph 
    type Model = {
        Variables: BmaVariable list
        Relationships: BmaRelationship list
    }

    // Layout metadata for visualising the BMA model in the web app 
    type Layout = {
        Variables: BmaVariable list
        Containers: obj list  // empty list in your JSON, so deserializes fine
    }

    // Full output of the graph in BMA-compatible format
    type GraphOutput = {
        Model: Model
        Layout: Layout
    }
open Z3optimisedDendogram

(*
GeneSimilarityMatrix: provides tools to
    - extract edges from BMA-format graph models 
    - map genes to their interacting edges
    - compute pairwise Jaccard similarity scores between genes based on shared edges
    - export the similarity matrix as a CSV distance matrix 
*)

module GeneSimilarityMatrix = 

    // Extracts a set of (source, target) edges from a BMA model 
    let extractEdgesFromModel (model:Model) : Set<string * string> = 
        model.Relationships
        |> List.map (fun r -> 
            let src = model.Variables |> List.find (fun v -> v.Id = r.FromVariable)
            let tgt = model.Variables |> List.find (fun v -> v.Id = r.ToVariable)
            src.Name, tgt.Name
        )
        |> List.filter (fun (src, tgt) -> src <> tgt) // remove self-edges
        |> Set.ofList

    // Parses a new-style BMA JSON file and returns a list of gene-to-edge mappings 
    // Each mapping represents a single gene and al (src, tgt) interactions it is involved in
    let extractGeneToEdgeMappings (filePath: string) : GeneEdgeMapping list =
        let json = File.ReadAllText(filePath).Trim()
        let options = JsonSerializerOptions()
        options.PropertyNameCaseInsensitive <- true

        try
            if json.StartsWith("[") then
                // JSON array of GraphOutput objects
                let graphs = JsonSerializer.Deserialize<GraphOutput list>(json, options)
                graphs
                |> List.mapi (fun i graph ->
                    let model = graph.Model
                    if obj.ReferenceEquals(model, null) then
                        failwithf "Model is null at index %d" i
                    let edges = extractEdgesFromModel model
                    printfn "Graph %d: Extracted edges count = %d" i (Set.count edges)
                    edges |> Set.iter (fun (src, tgt) -> printfn "  Edge: %s -> %s" src tgt)

                    let genes =
                        edges
                        |> Seq.collect (fun (src, tgt) -> [src; tgt])
                        |> Set.ofSeq
                    genes
                    |> Set.toList
                    |> List.map (fun gene ->
                        let relatedEdges =
                            edges |> Set.filter (fun (src, tgt) -> src = gene || tgt = gene)
                        { GraphId = i; Gene = gene; Edges = relatedEdges })
                )
                |> List.concat

            elif json.StartsWith("{") then
                // Single GraphOutput object
                let graph = JsonSerializer.Deserialize<GraphOutput>(json, options)
                let model = graph.Model
                if obj.ReferenceEquals(model, null) then
                    failwithf "Model is null in %s" filePath
                let edges = extractEdgesFromModel model
                let genes =
                    edges
                    |> Seq.collect (fun (src, tgt) -> [src; tgt])
                    |> Set.ofSeq
                genes
                |> Set.toList
                |> List.map (fun gene ->
                    let relatedEdges =
                        edges |> Set.filter (fun (src, tgt) -> src = gene || tgt = gene)
                    { GraphId = 0; Gene = gene; Edges = relatedEdges })

            else
                failwithf "Unrecognized JSON format in %s" filePath

        with ex ->
            failwithf "Failed to parse BMA graph from %s: %s" filePath ex.Message

    // Computes pairwise Jaccard similarity between all unique gene pairs
    let computeJaccardSimilarityMatrix (mergedEdges: Map<string, Set<string * string>>) : Map<string * string, float> =
        let genes = Map.keys mergedEdges |> Seq.toArray
        let offDiagonalScores = seq {
            for i = 0 to genes.Length - 1 do
                for j = i + 1 to genes.Length - 1 do
                    let g1, g2 = genes[i], genes[j]
                    let e1, e2 = mergedEdges[g1], mergedEdges[g2]
                    let inter = Set.intersect e1 e2 |> Set.count
                    let union = Set.union e1 e2 |> Set.count
                    let score = if union = 0 then 0.0 else float inter / float union
                    yield ((g1, g2), score)
                    yield ((g2, g1), score)
        }
        let diagonalScores = seq {
            for g in genes do
                yield ((g, g), 1.0)
        }
        // Combine off-diagonal and diagonal, diagonal overwrites if any conflict
        Map.ofSeq (Seq.append offDiagonalScores diagonalScores)


    // Export a similarity matrix as a CSV distance matrix file for Python plotting 
    let exportSimilarityMatrixCsv (similarityMatrix : Map<string * string, float>) (filename: string) = 
        // Extract all unique genes involved in the matrix and sort them alphabetically 
        let genes = 
            similarityMatrix
            |> Map.toSeq
            |> Seq.collect (fun ((g1, g2), _) -> [g1; g2])
            |> Seq.distinct 
            |> Seq.sort 
            |> Seq.toArray 

        let size = genes.Length

        // Create a 2D array to hold distances (distance = 1 - similarity)
        let distances = Array2D.create size size 0.0

        // Fill the distance matrix 
        for i in 0 .. size - 1 do 
            for j in 0 .. size - 1 do 
                if i = j then 
                    // Distance from a gene to itself is zero 
                    distances.[i,j] <- 0.0
                else
                    // Look up similarity score from the map 
                    // Try both (i,j) and (j,i) because similiarity is symmetric 
                    let sim = 
                        match similarityMatrix.TryFind (genes.[i], genes[j]) with 
                        | Some s -> s
                        | None -> 
                            match similarityMatrix.TryFind (genes.[j], genes.[i]) with 
                            | Some s2 -> s2
                            | None -> 0.0 // If no similarity found, assume zero similarity 
                    // Convert similarity to distance for clustering 
                    distances.[i,j] <- 1.0 - sim 
        // Write the matrix to a CSV file with gene names as headers and row labels
        use writer = StreamWriter(filename)
        
        // Write the header row (comma first, then gene names)
        writer.Write(",")
        writer.WriteLine(String.concat "," genes)

        //Write each row: gene name, then distance values
        for i in 0 .. size - 1 do
            // Create a string array: first element gene name, then all distances in the row formatted
            let row = 
                [| genes.[i] |]
                |> Array.append [| for j in 0 .. size - 1 -> sprintf "%.5f" distances.[i,j] |]
            // Write the CSV line 
            writer.WriteLine(String.concat "," row)
open GeneSimilarityMatrix

(*
DendrogramBuilder: uses hierarchical clustering to group genes based on their similarity, measured using Jaccard similarity of 
neighbouring edges in gene interaction graphs. 
    - rec flatten node: recursively extracts all gene names contained in a dendrogram node
    - buildDendrogram: takes a similarity matrix mapping pairs of genes and returns a dendrogram built by hierarchical clustering, by iteratively merging maxiimally 
    similar clusters 
    - processGraphFile: 
        extracts gene-to-edge mappings
        converts these mapping into a map
        computes the similarity matrix between genes 
        exports similarity matrix as a CSV 
        builds dendrogram from similarity matrix 
        returns the filename and the dendrogram 
    - processGraphFiles: applied processGraphFiles to multiple graph files, returning a list of (filename, dendrogram) tuples
    - printDendrogramSummaries: print a summary line per dendrogram, showing the total gene count in the root cluster 
*)

module DendrogramBuilder = 

    // Define the dendrogram node type used in hierarchical clustering 
    type DendroNode =
        | Leaf of string
        | Merge of DendroNode * DendroNode * float

    // Flatten a dendrogram node to get all gene names inside it
    let rec flatten node =
        match node with
        | Leaf g -> Set.singleton g
        | Merge (l, r, _) -> Set.union (flatten l) (flatten r)

    /// Builds a dendrogram using Jaccard similarity
    let buildDendrogram (similarityMatrix: Map<string * string, float>) : DendroNode =

        // Get all unique genes
        let genes =
            similarityMatrix
            |> Map.toSeq
            |> Seq.collect (fun ((g1, g2), _) -> [g1; g2])
            |> Seq.distinct
            |> Seq.toList

        // Build a map from gene to its neighbors (i.e., adjacency list)
        let neighbourMap =
            genes
            |> List.map (fun g ->
                let neighbors =
                    genes
                    |> List.filter (fun other ->
                        g <> other &&
                            match similarityMatrix.TryFind(g,other) with
                            | Some s when s > 0.0 -> true
                            | _ -> false)
                    |> Set.ofList
                g, neighbors)
            |> Map.ofList

        // Create initial clusters (each gene is a leaf node)
        let initialClusters =
            genes
            |> List.map (fun g -> g, Leaf g)
            |> Map.ofList

        // Define Jaccard similarity between two sets of genes
        let jaccardSimilarity (set1: Set<string>) (set2: Set<string>) =
                let n1 = set1 |> Seq.collect (fun g -> Map.find g neighbourMap) |> Set.ofSeq
                let n2 = set2 |> Seq.collect (fun g -> Map.find g neighbourMap) |> Set.ofSeq
                let unionSize = Set.union n1 n2 |> Set.count
                if unionSize = 0 then 0.0
                else
                    let intersectionSize = Set.intersect n1 n2 |> Set.count
                    float intersectionSize / float unionSize

        // Recursive merging loop
        let rec loop (clusters: Map<string, DendroNode>) : DendroNode =
            if Map.count clusters = 1 then
                let _, node = clusters |> Map.toSeq |> Seq.head
                node 
            else
                // All unique cluster pairs
                let clusterList = Map.toList clusters
                let clusterPairs =
                    [ for i in 0 .. clusterList.Length - 2 do
                        for j in i + 1 .. clusterList.Length - 1 do
                            yield clusterList[i], clusterList[j] ]

                // Score using Jaccard
                let scoredPairs =
                    clusterPairs
                    |> List.map (fun ((id1, n1), (id2, n2)) ->
                        let s1 = flatten n1
                        let s2 = flatten n2
                        let score = jaccardSimilarity s1 s2
                        ((id1, n1), (id2, n2), score))

                let ((idA, nA), (idB, nB), sim) = scoredPairs |> List.maxBy (fun (_, _, s) -> s)
                let newNode = Merge(nA, nB, sim)
                let newId = idA + "+" + idB

                loop (
                    clusters
                    |> Map.remove idA
                    |> Map.remove idB
                    |> Map.add newId newNode
                )

        loop initialClusters

    // Process a single graph file: extract the mappings, compute similarity, build dendrogram
    let processGraphFile (filePath: string) =
        printfn $"Processing {filePath} ..."

        // Extract gene-edge mappings from the graph JSON 
        let geneEdgeMappings = extractGeneToEdgeMappings filePath

        // Convert geneEdgeMappings (list) to Map<string, Set<string * string>>
        let mergedEdgesMap =
            geneEdgeMappings
            |> List.map (fun mapping -> mapping.Gene, mapping.Edges)
            |> Map.ofList

        printfn "Merged %d genes." (Map.count mergedEdgesMap)
        
        // Compute similarity matrix once
        let similarityMatrix = computeJaccardSimilarityMatrix mergedEdgesMap

        // Export the similarity matrix as CSV for Python visualisation 
        let csvFileName = filePath + "_similarity_matrix.csv"
        exportSimilarityMatrixCsv similarityMatrix csvFileName
        printfn $"Exported similarity matrix to {csvFileName}"

        // Build dendrogram from the similarity matrix 
        let dendrogram = buildDendrogram similarityMatrix
        
        // Return file and dendrogram for later use
        filePath, dendrogram

    // Process multiple graph files and return a list of (filename, dendrogram)
    let processGraphFiles (filePaths:string list) : (string * DendroNode) list =
        filePaths |> List.map processGraphFile

    // Print summary info for each dendrogram, gene count in each dendrogram leaf cluster root
    let printDendrogramSummaries (dendrograms: (string * DendroNode) list) : unit =
        dendrograms
            |> List.iter (fun (file, dendro) ->
                let genesInCluster = flatten dendro |> Set.count
                printfn $"File: {file}, genes in dendrogram root cluster: {genesInCluster}"
            )
open DendrogramBuilder

// list containing graph names
let graphFiles = 
    [ "graphs.json"
      "graphs_human_overlap_RAGEreceptors.json"
      "graphs_HSC_overlap_mouseRNAseq_final_consistent.json"
      "graphs_human_humanfinalConsistent.json" ]
    
(*
Main: asynchronous workflow to run graph processing tasks on multiple gene sets, writes summary 
CSVs with compressed graph outputs, and then processes dendrograms from a list of graph files, printing summaries

    NOTES ON FILE NAMES AND ADJUSTABILITY: 
        - the CSV and JSON filenames used for input and output are hardcoded as string literals

*)
module Main =
    let outputAsync = async {
        
        // Run all graphs for humanRAGEREceptors asynchronously
        
        let humanRAGEReceptors = loadGeneArrayFromCsv "human_overlap_RAGEreceptors.csv"
        let! outputs_humanRAGEReceptors = runAllWithGenes humanRAGEReceptors

        // Write the CSV summary with compressed BMA strings for human_overlap_RAGEreceptors gene list 
        writeGraphOutputCsvWithJsonReference "full_output_summary_human_overlap_RAGEreceptors.csv" "graphs_human_overlap_RAGEreceptors.json" outputs_humanRAGEReceptors
        printfn " CSV summary written to 'full_output_summary_human_overlap_RAGEreceptors.csv' "

        // Run all graphs for mouseRNAseq asynchronously
        
        let overlap_human_mouseRNAseq = loadGeneArrayFromCsv "HSC_overlap_mouseRNAseq_final_consistent.csv"
        let! outputs_mouseRNAseq = runAllWithGenes overlap_human_mouseRNAseq

        // Write the CSV summary with compressed BMA strings for mouseRNAseq gene list 
        writeGraphOutputCsvWithJsonReference "full_output_summary_HSC_overlap_mouseRNAseq_final_consistent.csv" "graphs_HSC_overlap_mouseRNAseq_final_consistent.json" outputs_mouseRNAseq 
        printfn " CSV summary written to 'full_output_summary_HSC_overlap_mouseRNAseq_final_consistent.csv' "
        
        // Run all graphs for humanfinalConsistent asynchronously
        
        let humanfinalConsistent = loadGeneArrayFromCsv "human_overlap_final_consistent.csv"
        let! outputs_humanfinalConsistent = runAllWithGenes humanfinalConsistent

        // Write the CSV summary with compressed BMA strings for humanfinalConsistent gene list 
        writeGraphOutputCsvWithJsonReference "full_output_summary_humanfinalConsistent.csv" "graphs_human_humanfinalConsistent.json" outputs_humanfinalConsistent
        printfn " CSV summary written to 'full_output_summary_human_humanfinalConsistent.csv' "

        // Run all graphs for mouseGenesMonika asynchronously 
        let! outputs = runAllWithGenes mouseGenesMonika

        // Write the CSV summary with compressed BMA strings 
        writeGraphOutputCsvWithJsonReference "full_output_summary.csv" "graphs.json" outputs
        printfn $"CSV summary written to full_output_summary.csv with %d{outputs.Length} graph(s)." 

    }

    // Process and print dendrograms 
    let dendrograms = processGraphFiles graphFiles
    printDendrogramSummaries dendrograms 
open Main 

outputAsync |> Async.RunSynchronously 
