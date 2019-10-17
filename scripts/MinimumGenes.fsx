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

open System
open System.IO
open System.Collections.Generic
open System.Windows.Forms

#load "getZ3.fsx"

#r "../platform/z3/bin/Microsoft.Z3.dll"
#r "../packages/Microsoft.Msagl/lib/net40/Microsoft.Msagl.dll"
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

#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data
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
type OmniPath = CsvProvider<"http://omnipathdb.org/interactions?fields=sources&fields=references&&genesymbols=1">

type OmniPathComplex = CsvProvider<"http://omnipathdb.org/complexes?&fields=sources,references">

//let omni = OmniPath.Load(interactionURL+"&genesymbols=1")
//let data = omni.Rows |> Seq.filter (fun x -> x.Is_directed && (x.Consensus_inhibition || x.Consensus_stimulation))

//let omniComplex = OmniPathComplex.Load("http://omnipathdb.org/complexes?&fields=sources,references")
//let complexData = omniComplex.Rows

type OmniSource = PPI | Regulon | PTM | MiRNA | Pathways | Combo

let getOmniData t strict =  let source = match t with 
                                            | PPI -> "http://omnipathdb.org/interactions?fields=sources&fields=references&genesymbols=1"
                                            | PTM -> "http://omnipathdb.org/interactions?datasets=kinaseextra&fields=sources&fields=references&genesymbols=1"
                                            | Regulon -> "http://omnipathdb.org/interactions?datasets=tfregulons&fields=sources&fields=references&genesymbols=1"
                                            | MiRNA -> "http://omnipathdb.org/interactions?datasets=mirnatarget&fields=sources,references&genesymbols=1"
                                            | Pathways -> "http://omnipathdb.org/interactions?datasets=pathwayextra&fields=sources,references&genesymbols=1"
                                            | Combo -> "http://omnipathdb.org/interactions?datasets=omnipath,pathwayextra,kinaseextra,ligrecextra,tfregulons,mirnatarget&fields=sources,references&genesymbols=1"
                            let data = OmniPath.Load(source)
                            let filter = if strict then (fun (x: OmniPath.Row) -> x.Is_directed && (x.Consensus_inhibition || x.Consensus_stimulation)) else (fun x -> true)
                            data.Rows |> Seq.filter filter

type PartialPath = 
    {
        parent : PartialPath option
        name :string
    } with 
    member this.ToArray = let rec core partial path = 
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


let pairwisePathSearch data genes includeSelfLoops=
    Array.collect (fun geneI -> Array.map (fun geneJ -> if includeSelfLoops || geneI <> geneJ then allShortestPaths geneI geneJ data else [||]) genes ) genes
    |> Array.filter (fun pSet -> pSet<>[||])

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

type GraphInput = {
    ctx : Context
    m : Model
    zGenes : Sort
    paths : string [] [] []
    genes : string []
    layout: LayoutSelection
    interactionInfo : Dictionary<string,InteractionInput> option
    maxEdges : Boolean
    s : Optimize
} with 
    member this.Next = 
        let variables = Array.map (fun geneName -> this.ctx.MkBoolConst(sprintf "Used-%s" geneName)) this.genes
        let values = Array.map (fun var -> this.m.Eval(var)) variables
        let constraints = Array.map2 (fun var value -> (this.ctx.MkEq(var,value))) variables values
        this.s.Add(this.ctx.MkNot(this.ctx.MkAnd(constraints)))
        match this.s.Check() with
        | Status.SATISFIABLE -> 
            printf "sat\n"
            //Return both inputs for makeGraphInternal to enable replotting
            Some({this with m=this.s.Model;s=this.s})
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            None
        | _ -> failwith "unknown"
    member this.Exclude genes = 
        let variables = Array.map (fun geneName -> this.ctx.MkBoolConst(sprintf "Used-%s" geneName)) genes
        let constraints = Array.map (fun var -> (this.ctx.MkEq(var,this.ctx.MkFalse()))) variables 
        this.s.Add(this.ctx.MkAnd(constraints))
        match this.s.Check() with
        | Status.SATISFIABLE -> 
            printf "sat\n"
            //Return both inputs for makeGraphInternal to enable replotting
            Some({this with m=this.s.Model;s=this.s})
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            None
        | _ -> failwith "unknown"

let fancyInteractions (data: Dictionary<string,InteractionInput> option) source target =
    match data with
    | None -> None
    | Some(iMap) -> 
        let key = sprintf "%s>%s" source target
        let information = iMap.[key]
        Some(information)

let makeGraphInternal gI =
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
    let bmaVariableLayout (bmaVar: BmaVariable) =
        sprintf "{\"Id\":%d,\"Name\":\"%s\",\"Type\":\"Constant\",\"ContainerId\":0,\"PositionX\":%f,\"PositionY\":%f,\"CellX\":0,\"CellY\":0,\"Angle\":0,\"Description\":\"%s\"}" bmaVar.id bmaVar.name bmaVar.x bmaVar.y bmaVar.description
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
            description = match nodeAnnotation.TryGetValue name with
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

    //convert nodes into appropriate strings
    let nodeCount = graph.Nodes.Count
    let nameToID = idMapping graph.Nodes
    //find the interactions first, to build up the description dictionary, then build the variables
    let interactions =  Array.ofSeq graph.Edges 
                        |> Array.mapi (fun i e -> edgeToBmaRel e nameToID (i+nodeCount) |> bmaRelationshipText) 
                        |> fun x -> String.Join(",",x)
    let bmaVariables = Array.init nodeCount (fun i -> nodeToBmaVar graph.Nodes.[i] i )
    let varModel = Array.map bmaVariableModel bmaVariables |> fun x -> String.Join(",",x) 
    let varLayout = Array.map bmaVariableLayout bmaVariables |> fun x -> String.Join(",",x) 

    let result = sprintf "{\"Model\": {\"Name\": \"Omnipath motif\",\"Variables\":[%s],\"Relationships\":[%s]},\"Layout\":{\"Variables\":[%s],\"Containers\":[]}}\n" varModel interactions varLayout
    Clipboard.SetText(result)
    printf "%s" result 

let makeGraph (ctx: Context) (m: Model) s zGenes paths genes layoutAlgo intData maxEdges =
    let gI = {
        ctx = ctx
        m = m
        zGenes = zGenes
        paths = paths
        genes = genes
        layout = layoutAlgo
        interactionInfo = intData
        maxEdges = maxEdges
        s = s
    }
    makeGraphInternal gI
    gI

let parsePath (line:string) = 
    line.Split '#'
    |> fun x -> x.[..((Array.length x) - 2)]
    |> Array.map (fun (p:String) -> p.Split ',')

let printGenes (ctx:Context) (m:Model) genes =
    let pairArray = Array.map (fun g-> (g,ctx.MkBoolConst(sprintf "Used-%s" g ))) genes
    let mutable used = 0
    for g,zg in pairArray do
        let state = sprintf "%O" (m.Eval(zg,true))
        if state = "true" then 
            printf "%s\n" g
            used <- used + 1
    printf "%d of %d genes used\n" used <| Array.length genes

type DataSource = FileName of string | Data of string [] [] []

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

let estimateComplexity source = 
    let data = match source with 
                | FileName(name) -> readFile name
                | Data(res) -> res
    if Array.sumBy Array.length data = 0 then 
        failwith "No paths found"
    Array.sumBy (Array.length >> float >> Math.Log10) data
    |> printf "10^%f alternatives to be searched\n"

type GeneData =  Demo | FromArray of string [] | FromFile of string

type MainInput = 
    {
        genesSource : GeneData
        includeSelfLoops : Boolean
        oneDirection : Boolean
        maximiseEdges : Boolean
        exclusions : string [] option
        database : OmniSource
        strictFilter : bool
    }

let defaultInput = 
    {
        genesSource = Demo
        includeSelfLoops = false
        oneDirection = true
        maximiseEdges = false
        exclusions = None
        database = PPI
        strictFilter = true
    }

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

let getPathsFromSource source data search selfLoops= 
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
        | FromFile(name) -> let arr = [| for line in File.ReadLines(name) do 
                                            yield line |]
                            search data arr selfLoops

let main input =
    let search = if input.oneDirection then OneDirectionalPathSearch else pairwisePathSearch
    let db = getOmniData input.database input.strictFilter
    let data =  match input.exclusions with
                        | None -> db
                        | Some(x) -> Seq.filter (fun (row:OmniPath.Row) -> Array.contains row.Source_genesymbol x || Array.contains row.Target_genesymbol x |> not ) db

    
    let (paths: string [] [] []) = getPathsFromSource input.genesSource data search input.includeSelfLoops
    let interactions =  match input.genesSource with 
                        | Demo -> None
                        | _ -> Some(buildEdgeDictionary paths data input.maximiseEdges)
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
            let result = makeGraph ctx s.Model s genes paths geneNames Sugiyama interactions input.maximiseEdges
            //Return both inputs for makeGraphInternal to enable replotting
            Some(result)
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            None
        | _ -> failwith "unknown"

type CrossTalkInput = 
    {
        genesSource1 : GeneData
        genesSource2 : GeneData
        includeSelfLoops : Boolean
        oneDirection : Boolean
        maximiseEdges : Boolean
        exclusions : string [] option
        database : OmniSource
        strictFilter : bool
    }

let defaultCrossTalkInput = 
    {
        genesSource1 = Demo
        includeSelfLoops = false
        oneDirection = true
        maximiseEdges = false
        exclusions = None
        genesSource2 = Demo
        database = PPI
        strictFilter = true
    }

let crossTalk input = 
    //Calculates the minimum distance between two sets of genes
    //Then calculates the mininum set of genes for each pathway, and required to connect the two pathways
    let search = if input.oneDirection then OneDirectionalPathSearch else pairwisePathSearch
    let db = getOmniData input.database input.strictFilter
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