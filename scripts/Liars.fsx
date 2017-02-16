#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

let main () = 
    let ctx = new Context()
    //We have two variables, A and B, and they are each either 2 or 1, or both 2, or both 1
    let nationalities = ctx.MkEnumSort("Nationalities",[|"Cretans";"EveryoneElse"|])
    let honesty = ctx.MkEnumSort("Truth",[|"Liar";"Honest"|])
    
    let speaker = ctx.MkConst("Speaker", honesty)
    let speakNationality = ctx.MkConst("Nationality",nationalities)
    
    let cretan = nationalities.Consts.[0]
    let others = nationalities.Consts.[1]

    let falsehood = honesty.Consts.[0]
    let truther = honesty.Consts.[1]

    //"All cretans are liars"
    let statement = ctx.MkAnd([|
                                ctx.MkImplies(ctx.MkEq(speakNationality,cretan),ctx.MkEq(speaker,falsehood))
                                ctx.MkImplies(ctx.MkEq(speaker,falsehood),ctx.MkFalse()) 
                                |])
    let paradox = ctx.MkEq(speakNationality,cretan)

    let s = ctx.MkSolver()
    s.Add([|statement;paradox|])

    match s.Check() with
    | Status.SATISFIABLE -> 
        printf "sat\n"
        printf "Speaker: %O\tNationality: %O\n" (s.Model.Eval(speaker,true)) (s.Model.Eval(speakNationality,true))
    | Status.UNSATISFIABLE -> printf "unsat"
    | _ -> failwith "unknown"

main ()