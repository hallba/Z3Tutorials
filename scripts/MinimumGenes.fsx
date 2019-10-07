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

let interactionURL = "http://omnipathdb.org/interactions?fields=sources&fields=references"
type OmniPath = CsvProvider<"http://omnipathdb.org/interactions?fields=sources&fields=references&&genesymbols=1">

let omni = OmniPath.Load(interactionURL+"&genesymbols=1")
let data = omni.Rows |> Seq.filter (fun x -> x.Is_directed && (x.Consensus_inhibition || x.Consensus_stimulation))

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

type xNode = 
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
        if (Array.isEmpty queue') then
            [||]
        else if (Array.length success > 0) then
            success
        else
            core queue' visited'
    core [|{name=source;parent=None}|] [||]
    |> Array.map (fun p -> p.ToArray)
//Uses paths and the ominpath data to create a dictionary of connections
let buildEdgeDictionary paths data =
    let getEdges (lookUp: Dictionary<string,int>) (vertices: xNode []) (d: Dictionary<string,InteractionInput>) path =
        let edgeNumber = Array.length path - 2
        for i=0 to edgeNumber do  
            let source = path.[i]
            let target = path.[i+1]
            let identifier = sprintf "%s>%s" source target
            match d.TryGetValue identifier with 
            | true, v -> ()
            | _ -> 
                let omniEdge = vertices.[lookUp.[source]].edges |> List.filter (fun x -> x.Target_genesymbol=target) |> List.last
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
    Array.iter (Array.iter (getEdges lookUp vertices edgeDescriptions)) paths
    edgeDescriptions


let pairwisePathSearch data genes =
    Array.collect (fun geneI -> Array.map (fun geneJ -> allShortestPaths geneI geneJ data) genes ) genes
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

type graphInput = {
    ctx : Context
    m : Model
    zGenes : Sort
    paths : string [] [] []
    genes : string []
    layout: LayoutSelection
    interactionInfo : Dictionary<string,InteractionInput> option
}

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

let makeGraph (ctx: Context) (m: Model) zGenes paths genes layoutAlgo intData =
    let gI = {
        ctx = ctx
        m = m
        zGenes = zGenes
        paths = paths
        genes = genes
        layout = layoutAlgo
        interactionInfo = intData
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

type dataSource = FileName of string | Data of string [] [] []

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
    Array.sumBy (fun x -> Array.length x |> float |> Math.Log10) data
    |> printf "10^%f alternatives to be searched\n"


let main inputGenes =
    let paths = match inputGenes with
                | None ->   [|
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
                | Some(arr) -> pairwisePathSearch data arr
    let interactions =  match inputGenes with 
                        | None -> None
                        | Some(_) -> Some(buildEdgeDictionary paths data)
    estimateComplexity <| Data(paths)
    let geneNames = Array.map Array.concat paths |> Array.concat |> Array.distinct

    let ctx = new Context()
    //This could be done with bitvectors and a manual numbering scheme but enumsort are converted anyway..
    let genes = ctx.MkEnumSort("genes",geneNames)
    let pairs = Array.mapi (fun i g -> (g,genes.Consts.[i] )) geneNames |> Map.ofArray

    let pathToConstraint (pairs: Map<string,Expr>) namer path  = 
        Array.mapi (fun i element -> ctx.MkEq((namer i),pairs.[element]) ) path
        |> fun x -> ctx.MkAnd(x)
    let createVariables (ctx:Context) (altPaths: string [] []) = 
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

    let s = ctx.MkOptimize()
    //let g = ctx.MkGoal()
    let varnames,vars = Array.map (createVariables ctx) paths 
                        |> fun x -> 
                            let names = Array.map fst x 
                            let behaviour = Array.map snd x
                            names,behaviour

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

    let countingElements = Array.map (geneUsed ctx genes varnames pairs) geneNames
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
            let result = makeGraph ctx s.Model genes paths geneNames Sugiyama interactions
            //Return both inputs for makeGraphInternal to enable replotting
            Some(result,interactions)
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            None
        | _ -> failwith "unknown"