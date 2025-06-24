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
#r "platform/z3/bin/Microsoft.Z3.dll"
#r "packages/Microsoft.Msagl.dll"
#r "System.IO.Compression.FileSystem.dll"

#load "getZ3.fsx"

open FSharp.Data
open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Diagnostics
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
    - 
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
GeneTypes: Defines types related to gene network modeling and input: variables, relationships, layout choices, 
input gene sources, main in put configurations
*)

module GeneTypes =
    type BmaVariable = {
                            id: int
                            name: string
                            x: float
                            y: float
                            formula: string
                            granularity: int
                            description: string
                        }
    type BmaRelationship = {
                                id: int
                                source: int
                                target: int
                                kind: string
                            }

    type LayoutSelection = MDS | Sugiyama | Ranking | FastIncremental

    type DataSource = FileName of string | Data of string [] [] []

    type GeneData =  Demo | FromArray of string [] | FromFile of string
    
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
    } 

    type MainInput = {
        genesSource : GeneData
        includeSelfLoops : Boolean
        oneDirection : Boolean
        maximiseEdges : Boolean
        exclusions : string [] option
        database : OmniSource
        strictFilter : bool
        hubGene : string option //all genes should be downstream of this if defined
        source: Organism
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

open GeneTypes

(*
GeneIO:    
    - Parse files for gene paths and interaction data 
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
    - createVariables and pathToConstraint: generating Z3 variables representing paths and constraining these to match 
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

    let makeGraph (ctx: Context) (m: Model) s zGenes paths genes layoutAlgo intData maxEdges input used=
        let (gI:GraphInput) = {
            ctx = ctx
            m = m
            zGenes = zGenes
            paths = paths
            genes = genes
            layout = layoutAlgo
            interactionInfo = intData
            maxEdges = maxEdges
            s = s
            rotation = 0.
            inputGenes = input
            numberGenesUsed = used
        }
        makeGraphInternal gI
        gI

    let graphInputNext (this: GraphInput) =
        let variables = Array.map (fun geneName -> this.ctx.MkBoolConst(sprintf "Used-%s" geneName)) this.genes
        let values = Array.map (fun var -> this.m.Eval(var)) variables
        let constraints = Array.map2 (fun var value -> (this.ctx.MkEq(var,value))) variables values
        this.s.Add(this.ctx.MkNot(this.ctx.MkAnd(constraints)))
        match this.s.Check() with
        | Status.SATISFIABLE -> 
            printf "sat\n"
            printGenes this.ctx this.s.Model this.genes
            let used = countUsedGenes this.ctx this.genes this.s.Model
            let result = makeGraph this.ctx this.s.Model this.s this.zGenes this.paths this.genes this.layout this.interactionInfo this.maxEdges this.inputGenes used
            Some({ this with m = this.s.Model; s = this.s; numberGenesUsed = used })
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            None
        | _ -> failwith "unknown"

    let graphInputExclude (this: GraphInput) (genes: string[]) =
        let variables = Array.map (fun geneName -> this.ctx.MkBoolConst(sprintf "Used-%s" geneName)) genes
        let constraints = Array.map (fun var -> (this.ctx.MkEq(var,this.ctx.MkFalse()))) variables 
        this.s.Add(this.ctx.MkAnd(constraints))
        match this.s.Check() with
        | Status.SATISFIABLE -> 
            printf "sat\n"
            Some({ this with m = this.s.Model; s = this.s })
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            None
        | _ -> failwith "unknown"
        
    let findAllEquivalentGraphs (g:  GraphInput) = 
        let rec core (g: GraphInput) acc =
            match graphInputNext g with 
            | None -> acc
            | Some(g') -> core g' (g'::acc)
        g.s.Push()
        let gc = g.ctx.MkIntConst("GeneCount")
        let modelScore = sprintf "%O" (g.m.Eval(gc,true) )
        let score = g.ctx.MkInt(modelScore)
        g.s.Add(g.ctx.MkEq(gc,score))
        let result = core g [g]
        g.s.Pop()
        result

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

                    let result = makeGraph ctx s.Model s genes paths geneNames Sugiyama interactions input.maximiseEdges inputGenes used
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
                    let result = makeGraph ctx s.Model s genes paths geneNames Sugiyama interactions input.maximiseEdges
                    //Return both inputs for makeGraphInternal to enable replotting
                    Some(result)
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

let mouseGenesMonika = [| "Agps";"Coro7";"Epdr1";"Fth1";"Ftl1";"Mocs3";"Rap2b";"Serpina1a";"Sh3bp1";"Slc14a1";"Tgm2";"Upp1"|]
//let mouse1: obj = main {defaultInput with genesSource=FromArray mouseGenesMonika;database=Combo;source=Mouse}
let mouse2: obj = main {defaultInput with genesSource=FromArray(mouseGenesMonika);database=PPI;source=Mouse}
//let mouse3: obj = main {defaultInput with genesSource=FromArray(mouseGenesMonika);database=Regulon;source=Mouse}

(*// Define different configurations that override initial config
let config1 = { defaultInput with genesSource=FromArray mouseGenesMonika;database = PPI }
let config2 = { defaultInput with genesSource=FromArray mouseGenesMonika;oneDirection = false }
let config3 = { defaultInput with genesSource=FromArray mouseGenesMonika;includeSelfLoops = true }

let res1 = main config1 |> ignore
let res2 = main config2 |> ignore
let res3 =main config3 |> ignore

res1
*)

(*
Tests all possible combinations of parameters using a fixed set of genes, and for each 
valid configuration that produces a graph, it generates the minimal gene interaction graph using the 
main solver and sends the result to the clipboard
*)
let allOptions =
    [
        for selfLoops in [true; false] do
        for oneDir in [true; false] do
        for maxEdges in [true; false] do
        for strictFilter in [true; false] do
        for db in [PPI; Regulon; PTM; MiRNA; Pathways; Combo] do
        for source in [Human; Mouse; Rat] do
            yield {
                defaultInput with
                    includeSelfLoops = selfLoops
                    oneDirection = oneDir
                    maximiseEdges = maxEdges
                    strictFilter = strictFilter
                    database = db
                    source = source
            }
    ]

// defines a function called runAllWithGenes that takes a parameter genes 
let runAllWithGenes genes = 
    // start a loop over every configuration in allOptions list, containing every combination of parameters 
    for config in allOptions do 
        // creates a new config record by copying config but overriding the genesSource field with specific gene list wrapped in FromArray. 
        let configWithGenes = {config with genesSource = FromArray(genes)}

        // call main solver function, passing the config with the gene list 
        match main configWithGenes with 

        // None if no solution exists
        | None -> printfn "No graph found for config: %A" configWithGenes
        | Some(_) -> printfn "Graph generated and sent to clipboard for config: %A" configWithGenes

runAllWithGenes mouseGenesMonika

fsi.ShowDeclarationValues <- false 

(* modified runAllWithGenes with additional step to collect all graphs into a list and allow user to select graph by index, 
then send the selected graph string to the clipboard*)
// Main interactive runner function
let runAllWithGenesInteractive (genes: string[]) =
    // Mutable list to store all generated graph strings
    let allGraphs = new System.Collections.Generic.List<string>()

    // Iterate over each configuration with index
    for i, config in allOptions |> List.mapi (fun i c -> (i, c)) do
        // Override genesSource field with provided genes array
        let configWithGenes = { config with genesSource = FromArray(genes) }
        
        printfn "[%d/%d] Trying config: %A" (i + 1) allOptions.Length configWithGenes

        // Run the solver (your 'main' function) on the config
        match main configWithGenes with
        | None -> printfn "No graph found for this configuration."
        | Some graphInput ->
            // Convert GraphInput to string (BMA or JSON) using your function
            let graphStr = makeGraphInternal graphInput
            printfn "Graph generated for config %d" (i + 1)
            allGraphs.Add(graphStr)


    // Convert to immutable list
    let graphs = allGraphs |> Seq.toList

    if graphs.Length = 0 then
        printfn "No graphs generated."
    else
        // Display all graphs with index and preview (first 100 chars)
        printfn "\nGenerated %d graphs. Select one by index (0 to %d):" graphs.Length (graphs.Length - 1)
        graphs |> List.iteri (fun i g -> 
            let preview = if g.Length > 100 then g.Substring(0, 100) + "..." else g
            printfn "[%d]: %s" i preview)

        // Prompt user input
        printf "Enter index of graph to copy to clipboard: "
        let input = Console.ReadLine()

        // Parse input and send selected graph to clipboard
        match Int32.TryParse(input) with
        | (true, idx) when idx >= 0 && idx < graphs.Length ->
            let selectedGraph = graphs.[idx]
            sendToClipboard selectedGraph
            printfn "Graph %d copied to clipboard." idx
        | _ ->
            printfn "Invalid selection, no graph copied."

runAllWithGenesInteractive mouseGenesMonika