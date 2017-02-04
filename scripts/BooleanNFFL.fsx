#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

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

let findFixpoint (ctx:Context) =
    let s = ctx.MkSolver()
    step ctx s 0 0
    match s.Check() with
    | Status.SATISFIABLE -> printf "Found Fixpoint\n"
                            ignore(List.map (fun name -> printf "%s:\t%O\n" name (s.Model.Eval(makeVariable ctx name 0)) ) ["Input";"A";"B";"Output"])
    | Status.UNSATISFIABLE -> printf "No fixpoints\n"
    | _ -> failwith "Unknown result from fixpoint search"

let main _ = 
    let ctx = new Context()
    findFixpoint ctx
