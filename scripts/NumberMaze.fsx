#r "nuget: FSharp.Data"
#r "platform/z3/bin/Microsoft.Z3.dll"
#r "packages/Microsoft.Msagl.dll"
#r "System.IO.Compression.FileSystem.dll"

#load "getZ3.fsx"

open Microsoft.Z3  

let ctx = new Context()

let makeVarPath x y =
    ctx.MkBoolConst(sprintf "var-%d-%d-path" x y)

let makeVarValue x y =
    ctx.MkIntConst(sprintf "var-%d-%d-value" x y)

let zTrue =
    ctx.MkTrue()

let adjacent x y (maxI:int) =
    match x,y with
    //four corners only have 2 adjacent
    | 0,0 -> [| (1,0); (0,1)|] 
    | px,py when px = maxI && py = maxI -> [| (maxI-1,maxI); (maxI,maxI-1)|] 
    | 0,p when p = maxI -> [| (1,maxI); (0,maxI-1)|] 
    | p,0 when p = maxI -> [| (maxI,1); (maxI-1,0)|] 
    //Edge cells that are not corners have 3 adjacent
    | 0,_ -> [|(x+1,y);(x,y-1);(x,y+1)|]
    | _,0 -> [|(x-1,y);(x+1,y);(x,y+1)|]
    | _,p when p = maxI -> [|(x-1,y);(x+1,y);(x,y-1)|]
    | p,_ when p = maxI -> [|(x-1,y);(x,y-1);(x,y+1)|]
    //Everything else has four corners
    | _,_ -> [|(x-1,y);(x+1,y);(x,y-1);(x,y+1)|]

let printMap (m:Model) size tt = 
    for x in 0..(size-1) do
        for y in 0..(size-1) do
            //printf "%d-%d " x y 
            printf "%2d-%2d " x y 
        printf "\n"

    for x in 0..(size-1) do
        for y in 0..(size-1) do
            //printf "%d-%d " x y 
            m.Eval((makeVarValue x y ),true)
            |> sprintf "%O" 
            |> fun x -> printf "%5d " ((int x) * tt)
        printf "\n"

    for x in 0..(size-1) do
        for y in 0..(size-1) do
            //printf "%d-%d " x y 
            m.Eval((makeVarPath x y ),true)
            |> printf "%5O " 
        printf "\n"

let main size tt = 
    let maxIndex = size - 1

    let paths = Array.init size (fun x -> Array.init size (fun y -> makeVarPath x y))
                |> Array.map (fun t -> ctx.MkOr(t))
                |> fun t -> ctx.MkOr(t)
    let valuesMax = Array.init size (fun x -> Array.init size (fun y -> ctx.MkLt(makeVarValue x y,ctx.MkInt(size*size))))
                    |> Array.map (fun t -> ctx.MkAnd(t))
                    |> fun t -> ctx.MkAnd(t)
    let valuesMin = Array.init size (fun x -> Array.init size (fun y -> ctx.MkGe(makeVarValue x y,ctx.MkInt(0))))
                    |> Array.map (fun t -> ctx.MkAnd(t))
                    |> fun t -> ctx.MkAnd(t)
    

    //Unique values for each value
    let distinct = Array.init size (fun x -> Array.init size (fun y -> makeVarValue x y))
                |> Array.concat
                |> Array.map (fun x -> x :> Expr)
                |> fun x -> ctx.MkDistinct(x)

    //Starts at zero, and the path starts and ends at the origin and the opposite coordinate
    let startPoint = ctx.MkAnd(ctx.MkEq(makeVarPath 0 0, zTrue),ctx.MkEq(makeVarValue 0 0,ctx.MkInt(0)))
    //ctx.MkEq(makeVarValue 0 0,ctx.MkInt((size*size)-1))
    //How do I make the endpoint be in the corner? maximising it wont work
    let endPoint = ctx.MkEq(makeVarPath maxIndex maxIndex, zTrue)

    //if a variable is a path either it is the start or end, or it is 
    //adjacent a variable which is a path and is one less than itself
    //Path steps should have only one predecessor

    let checkAdjacentPred x y =
        let neighboursValues = adjacent x y maxIndex 
                                |> Array.map (fun a -> makeVarValue (fst a) (snd a))
        let myValue = makeVarValue x y 
        Array.map (fun n -> ctx.MkEq(ctx.MkAdd(n,ctx.MkInt(1)),myValue)) neighboursValues

    let checkAdjacentSucc x y = 
        let neighboursValues = adjacent x y maxIndex 
                                |> Array.map (fun a -> makeVarValue (fst a) (snd a))
        let myValue = makeVarValue x y 
        Array.map (fun n -> ctx.MkEq(ctx.MkSub(n,ctx.MkInt(1)),myValue)) neighboursValues

    let xorArray a =
        //One must be true
        let one = ctx.MkOr(a)
        //Others must be false
        //test by making pairwise comparisons
        let len = Array.length a

        let pairwise = Array.init len (fun i -> Array.init i (fun j -> ctx.MkAnd(a.[i],a.[j]))) 
                        |> Array.concat
                        |> fun x -> ctx.MkOr(x)

        ctx.MkAnd(one,ctx.MkNot(pairwise))

    //ctx.MkImplies(makeVarPath 2 2,xorArray(checkAdjacentPred 2 2))
    
    //If I'm on the path, I must have a predecessor adjacent
    let predRule x y =
            //Special rule- the origin does not have a predecessor
            if x = 0 && y = 0 then
                zTrue
            else
                ctx.MkImplies(makeVarPath x y,xorArray(checkAdjacentPred x y))
    let predecessor = //ctx.MkImplies(makeVarPath 2 2,xorArray(checkAdjacentPred 2 2))
        Array.init size (fun i -> Array.init size (fun j -> predRule i j)) 
        |> Array.concat
        |> fun x -> ctx.MkAnd(x)
    //If I have a successor adjacent on the path, I am on the path
    


    let succFind x y =
        adjacent x y maxIndex 
        |> Array.map (fun a -> (makeVarPath (fst a) (snd a)),(makeVarValue (fst a) (snd a)))

    let succRule x y =
        if x = maxIndex && y = maxIndex then
            zTrue
        else
            //ctx.MkOr(succFind x y)
            //succFind x y
            //|> Array.map (fun (p,v) -> checkAdjacentSucc x y )
            //|> ignore
            //zTrue
            ctx.MkAnd(makeVarPath x y, ctx.MkOr(checkAdjacentSucc x y))

    let successor   = //ctx.MkImplies(makeVarPath 1 1,ctx.MkOr(succFind 1 1))
        Array.init size (fun i -> Array.init size (fun j -> succRule i j)) 
        |> Array.concat
        |> fun x -> ctx.MkAnd(x)

    let s = ctx.MkSolver()
    s.Add([|
        startPoint;
        endPoint;
        paths;
        valuesMax;
        valuesMin;
        //distinct;
        predecessor;
        successor;
        //ctx.MkEq(makeVarValue 1 2, ctx.MkInt(4));
        //ctx.MkEq(makeVarValue 2 1, ctx.MkInt(3));
        |])

    match s.Check() with
    | Status.SATISFIABLE -> 
        printf "sat\n"
        printMap s.Model size tt
        Some(s.Model)
        //printf "Speaker: %O\tNationality: %O\n" (s.Model.Eval(speaker,true)) (s.Model.Eval(speakNationality,true))
    | Status.UNSATISFIABLE -> 
        printf "unsat"
        None
    | _ -> failwith "unknown"

main 3 5

