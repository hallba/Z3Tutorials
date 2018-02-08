#load "getZ3.fsx"

#r "../platform/z3/bin/Microsoft.Z3.dll"

open Microsoft.Z3 

//Prove stability in a negative feedforward loop (Perfect adaption motif)
// Input -> A
// A -> B 
// A -> Output
// B -| Output
let makeVariable (ctx:Context) name time =
    ctx.MkBoolConst(sprintf "%s-%d" name time)

let step (ctx:Context) (s:Solver) t t' = 
    //Update input 
    let input' = ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkTrue())
    //Update A 
    let a' = ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "Input" t))
    //Update B
    let b' = ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "A" t))
    //Update Output
    let output' =   ctx.MkAnd(  [|
                                ctx.MkImplies(ctx.MkEq((makeVariable ctx "A" t),(ctx.MkFalse())),ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkFalse()))
                                ctx.MkImplies(ctx.MkAnd(ctx.MkEq((makeVariable ctx "A" t),ctx.MkTrue()),ctx.MkEq((makeVariable ctx "B" t),(ctx.MkFalse()))),ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkTrue()))
                                ctx.MkImplies(ctx.MkAnd(ctx.MkEq((makeVariable ctx "A" t),ctx.MkTrue()),ctx.MkEq((makeVariable ctx "B" t),(ctx.MkTrue()))),ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkFalse()))
                                |]  )
    s.Add(ctx.MkAnd([|input'; a'; b'; output'|]))

let excludeState (ctx:Context) (s:Solver) t =
    s.Add(ctx.MkNot(ctx.MkAnd(
        [|
        ctx.MkEq(s.Model.Eval(makeVariable ctx "Input" t),makeVariable ctx "Input" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "A" t),makeVariable ctx "A" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "B" t),makeVariable ctx "B" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "Output" t),makeVariable ctx "Output" t)
        |]
    )))

type result = Bifurcation | NoBifurcation | Cycle | Stable 

let findFixpoints _ =
    let ctx = new Context()    
    let s = ctx.MkSolver()
    step ctx s 0 0
    match s.Check() with
    | Status.SATISFIABLE -> printf "Found Fixpoint\n"
                            ignore(List.map (fun name -> printf "%s:\t%O\n" name (s.Model.Eval(makeVariable ctx name 0)) ) ["Input";"A";"B";"Output"])
                            excludeState ctx s 0
                            match s.Check() with
                            | Status.UNSATISFIABLE -> printf "Only a single fixpoint\n"; NoBifurcation
                            | Status.SATISFIABLE -> printf "Found bifurcation\n"; Bifurcation
                            | _ -> failwith "Unknown result from Z3"
    | Status.UNSATISFIABLE -> printf "No fixpoints\n"; NoBifurcation
    | _ -> failwith "Unknown result from fixpoint search"

let statesEqual (ctx:Context) t t' =
    ctx.MkAnd(
        [|
        ctx.MkEq(makeVariable ctx "Input" t',makeVariable ctx "Input" t)
        ctx.MkEq(makeVariable ctx "A" t',makeVariable ctx "A" t)
        ctx.MkEq(makeVariable ctx "B" t',makeVariable ctx "B" t)
        ctx.MkEq(makeVariable ctx "Output" t',makeVariable ctx "Output" t)
        |]
    )

let statesAreEqual ctx (s:Solver) t t' = 
    s.Add(statesEqual ctx t t')
let statesAreNotEqual (ctx:Context) (s:Solver) t t' =
    s.Add(ctx.MkNot(statesEqual ctx t t'))

let findCycles bound =
    let ctx = new Context()
    let s = ctx.MkSolver()
    printf "Searching for cycles at bound"
    let rec core i bound =
        printf "...%d" i;
        step ctx s (i-1) i
        s.Push()
        //Need to assert that first state is not a fixpoint
        statesAreNotEqual ctx s 0 1
        statesAreEqual ctx s 0 i
        match s.Check() with 
        | Status.SATISFIABLE -> printf "Found cycle of length %d\n" i; Cycle
        | Status.UNSATISFIABLE -> s.Pop(); if i < bound then core (i+1) bound else printf "\n"; Stable
        | _ -> failwith "Unknown result"
    core 1 bound


let main _ = 
    match findFixpoints () with
    | Bifurcation -> ()
    | _ -> match findCycles 16 with
            | Cycle -> ()
            | Stable -> printf "Model is stable!\n"
            | _ -> failwith "problem- error"
