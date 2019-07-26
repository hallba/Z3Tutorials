(*
Three gene pairs, UV, WX,YZ
Each has the following shortest paths

U A B C V
U A B D V
U E F G V

W A X

Y D Z
Y E Z

Want to find the smallest number of genes that link the original set
 U V W X Y Z

 *)

open System
open System.Net
open System.IO

#load "getZ3.fsx"

#r "../platform/z3/bin/Microsoft.Z3.dll"

open Microsoft.Z3 

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

let main fromFile =
    let paths = match fromFile with
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
                | Some(name) -> let mutable pathcount = 0
                                let result = [|for line in File.ReadLines(name) do 
                                                let paths = parsePath line
                                                pathcount <- pathcount + (Array.length paths)
                                                yield paths|]
                                printf "Reducing %d possible paths to %d paths\n" pathcount (Array.length result)
                                result

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
                            let names = Array.collect fst x 
                            let behaviour = (ctx.MkAnd(Array.map snd x))
                            names,behaviour

    //Has a gene been used? 
    let geneUsed (ctx:Context) genes pathVars (pairMap:Map<string,Expr>) geneName =
        let usedVar = ctx.MkBoolConst(sprintf "Used-%s" geneName)
        let zGene = pairMap.[geneName]
        let used = Array.map (fun n -> ctx.MkEq(zGene,n)) pathVars |> fun x -> ctx.MkOr(x)
        ctx.MkEq(usedVar,used)

    let countingElements = Array.map (geneUsed ctx genes varnames pairs) geneNames
                           |> fun x -> ctx.MkAnd(x)

    let geneNumber = ctx.MkIntConst("GeneCount")
    let boolToNumber (ctx: Context) n =
        ctx.MkITE(ctx.MkBoolConst(sprintf "Used-%s" n), ctx.MkInt(1),ctx.MkInt(0)) :?> ArithExpr
    let countGenes = Array.map (fun n -> boolToNumber ctx n) geneNames  |> fun x -> ctx.MkAdd(x)
    let measure = ctx.MkEq(geneNumber,countGenes)

    s.Add(vars)   
    s.Add(countingElements) 
    s.Add(measure)
    ignore <| s.MkMinimize(geneNumber)
    //printf "%s\n" <| s.ToString()
    match s.Check() with
        | Status.SATISFIABLE -> 
            printf "sat\n"
            //printf "%s\n" <| s.Model.ToString()
            printGenes ctx s.Model geneNames
            ()//Some(s.Model)
        | Status.UNSATISFIABLE -> 
            printf "unsat"
            ()//None
        | _ -> failwith "unknown"