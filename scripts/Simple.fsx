#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

let sanity_check (ctx:Context) (s:Solver) (v:IntExpr []) =
    s.Push()
    let r = s.Check([||])
    match r with 
    | Status.UNSATISFIABLE ->
        s.Pop()
        sprintf "FAIL sanity test- unsat\n"
    | Status.SATISFIABLE -> 
        let a = sprintf "PASS sanity test\nA %O B %O\n" (s.Model.ConstInterp(v.[0])) (s.Model.ConstInterp(v.[1]))
        s.Pop()
        a
    | _ -> failwith "Unknown fail"

let paradox (ctx:Context) (s:Solver) (variables:IntExpr []) =
    s.Push()
    let additionalConstraints = [|ctx.MkAnd([|ctx.MkEq(variables.[0],ctx.MkInt(2));ctx.MkEq(variables.[0],ctx.MkInt(1))|])|]
    s.Add(additionalConstraints)
    let r = s.Check([||])
    match r with 
    | Status.UNSATISFIABLE ->
        s.Pop()
        sprintf "PASS paradox test- unsat\n"
    | Status.SATISFIABLE -> 
         let result = sprintf "FAIL sanity test\nA %O B %O\n" (s.Model.ConstInterp(variables.[0])) (s.Model.ConstInterp(variables.[1]))
         s.Pop()
         result
    | _ -> failwith "Unknown fail"

let answer (ctx:Context) (s:Solver) (variables:IntExpr []) =
    s.Push()
    let additionalConstraints = [|ctx.MkLt(variables.[0],variables.[1])|]
    s.Add(additionalConstraints)
    let r = s.Check([||])
    match r with 
    | Status.UNSATISFIABLE ->
        s.Pop()
        "No answer- unsat\n"
    | Status.SATISFIABLE -> 
        let aState = s.Model.ConstInterp(variables.[0])
        let bState = s.Model.ConstInterp(variables.[1])
        let result = sprintf  "Answer- sat\nA %O B %O\n" aState bState
        s.Add (ctx.MkNot(ctx.MkAnd(ctx.MkEq(variables.[0],aState),ctx.MkEq(variables.[1],bState))))
        let r' = s.Check([||])
        match r' with
        | Status.UNSATISFIABLE ->
            s.Pop()
            result + "No other solutions\n"
        | Status.SATISFIABLE ->
            s.Pop()
            result + (sprintf "Alternative answer\nA %O B %O\n" (s.Model.ConstInterp(variables.[0])) (s.Model.ConstInterp(variables.[1])))
        | _ -> failwith "Unknown fail"
    | _ -> failwith "Unknown fail"
let main () = 
    let ctx = new Context()
    //We have two variables, A and B, and they are each either 2 or 1, or both 2, or both 1
    let variables = [|ctx.MkIntConst("A");ctx.MkIntConst("B")|]
    let values = [|ctx.MkInt(2);ctx.MkInt(1)|]
    let constraints = [|
        ctx.MkOr([|ctx.MkEq(variables.[0],values.[0]);ctx.MkEq(variables.[0],values.[1])|]);
        ctx.MkOr([|ctx.MkEq(variables.[1],values.[0]);ctx.MkEq(variables.[1],values.[1])|]);
        |]
    let s = ctx.MkSolver()
    s.Add(ctx.MkAnd(constraints))
    //Must be satisfiable
    printf "%s" (sanity_check ctx s variables)
    //Paradox- must not be satisfiable
    printf "%s" (paradox ctx s variables)
    //A is less than B- What is the answer?
    printf "%s" (answer ctx s variables)