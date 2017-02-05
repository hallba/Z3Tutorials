#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

//Simplified VPC model with only two cells
//Each cell has IS, LS, Notch, MAPK, LET60, Fate, Moved genes
//The activity of Notch is determined by the activity of LS *in the neighbouring cell*

let geneCreate (ctx:Context) name t position states =
    ctx.MkConst((sprintf "%s-%d-%d" name t position),states)

let cellUpdate (ctx:Context) (states:EnumSort) (fates:EnumSort) t t' position altPosition = 
    let notch = geneCreate ctx "Notch" t position states
    let notch' = geneCreate ctx "Notch" t' position states
    let is = geneCreate ctx "IS" t position states
    let is' = geneCreate ctx "IS" t' position states
    let let60 = geneCreate ctx "LET60" t position states
    let let60' = geneCreate ctx "LET60" t' position states
    let mapk = geneCreate ctx "MAPK" t position states
    let mapk' = geneCreate ctx "MAPK" t' position states
    let ls = geneCreate ctx "LS" t position states
    let ls' = geneCreate ctx "LS" t' position states

    let fate = geneCreate ctx "Fate" t position fates
    let fate' = geneCreate ctx "Fate" t' position fates
    // let move = ctx.MkBoolConst(sprintf "Move-%d-%d" t position)
    // let move' = ctx.MkBoolConst(sprintf "Move-%d-%d" t' position)
    //LS from other cell
    let lsN = geneCreate ctx "LS" t altPosition states
    
    let let60Update = ctx.MkEq(let60',is) 
    let isUpdate = ctx.MkEq(is',states.Consts.[0])
    let notchUpdate = ctx.MkEq(notch',lsN)
    let lsUpdate = ctx.MkEq(ls',mapk)
    let mapkUpdate =   ctx.MkAnd( [|
                                    //No notch inhibition
                                    ctx.MkImplies(ctx.MkEq(let60,states.Consts.[0]),ctx.MkEq(mapk',states.Consts.[0]))
                                    ctx.MkImplies(ctx.MkEq(let60,states.Consts.[1]),ctx.MkEq(mapk',states.Consts.[1]))
                                    ctx.MkImplies(ctx.MkEq(let60,states.Consts.[2]),ctx.MkEq(mapk',states.Consts.[2]))
                        |] )
    
    let fateUpdate =    ctx.MkAnd([|
                                    //Once a fate is assigned, never change
                                    ctx.MkImplies(ctx.MkNot(ctx.MkEq(fates.Consts.[0],fate)),ctx.MkEq(fate',fate))
                                    //MAPK is active -> Primary
                                    ctx.MkImplies(ctx.MkAnd(ctx.MkEq(fates.Consts.[0],fate),ctx.MkEq(mapk,states.Consts.[2])),ctx.MkEq(fate',fates.Consts.[1]))
                                    //Notch is active -> Secondary
                                    ctx.MkImplies(ctx.MkAnd(ctx.MkEq(fates.Consts.[0],fate),ctx.MkNot(ctx.MkEq(mapk,states.Consts.[2])),ctx.MkEq(notch,states.Consts.[2])),ctx.MkEq(fate',fates.Consts.[2]))
                                    //Time runs out -> Tertiary
                                    //ctx.MkImplies(ctx.MkAnd(ctx.MkEq(fates.Consts.[0],fate),ctx.MkNot(ctx.MkEq(mapk,states.Consts.[2])),ctx.MkNot(ctx.MkEq(notch,states.Consts.[2])),ctx.MkEq(ctx.MkIntConst(sprintf "Clock-%d" t ),ctx.MkInt(5))),ctx.MkEq(fate',fates.Consts.[2]))
                                    ctx.MkImplies(  ctx.MkAnd(
                                                                ctx.MkEq(ctx.MkIntConst(sprintf "Clock-%d" t ),ctx.MkInt(5)),
                                                                ctx.MkNot(ctx.MkEq(mapk,states.Consts.[2])),
                                                                ctx.MkNot(ctx.MkEq(notch,states.Consts.[2]))
                                                                ),ctx.MkEq(fate',fates.Consts.[3]))
                                    ctx.MkImplies(  ctx.MkAnd(
                                                                ctx.MkNot(ctx.MkEq(ctx.MkIntConst(sprintf "Clock-%d" t ),ctx.MkInt(5))),
                                                                ctx.MkNot(ctx.MkEq(mapk,states.Consts.[2])),
                                                                ctx.MkNot(ctx.MkEq(notch,states.Consts.[2]))
                                                                ),ctx.MkEq(fate',fate))
                                    |])
    ctx.MkAnd([|let60Update;isUpdate;notchUpdate;lsUpdate;mapkUpdate;fateUpdate|])

let step (ctx:Context) (s:Solver) (states:EnumSort) (fates:EnumSort) t t' =
    let clock  = ctx.MkIntConst(sprintf "Clock-%d" t )
    let clock' = ctx.MkIntConst(sprintf "Clock-%d" t')
    let timer = ctx.MkEq(clock',ctx.MkAdd(clock,ctx.MkInt(1)))
    //Variables in cell 0
    let cell0Update = cellUpdate ctx states fates t t' 0 1
    let cell1Update = cellUpdate ctx states fates t t' 1 0
    s.Add(ctx.MkAnd([|timer;cell0Update;cell1Update|]))

let initCell (ctx:Context) position t states initState fates initFate =
    let notch = geneCreate ctx "Notch" t position states
    let is = geneCreate ctx "IS" t position states
    let let60 = geneCreate ctx "LET60" t position states
    let mapk = geneCreate ctx "MAPK" t position states
    let ls = geneCreate ctx "LS" t position states
    let fate = geneCreate ctx "Fate" t position fates
    ctx.MkAnd([|
                ctx.MkEq(notch,initState)
                ctx.MkEq(is,initState)
                ctx.MkEq(let60,initState)
                ctx.MkEq(mapk,initState)
                ctx.MkEq(ls,initState)
                ctx.MkEq(fate,initFate)
    |])

let init (ctx:Context) (s:Solver) (states:EnumSort) t initState (fates:EnumSort) initFate =
    let cell0 = initCell ctx 0 t states initState fates initFate
    let cell1 = initCell ctx 1 t states initState fates initFate
    let timer = ctx.MkEq((ctx.MkIntConst(sprintf "Clock-%d" t )),ctx.MkInt(0))
    s.Add(ctx.MkAnd([|cell0;cell1;timer|]))

let main bound = 
    let ctx = new Context ()
    let genes = [|"IS";"LS";"Notch";"LET60";"MAPK";"Fate";"Moved"|]
    let activity = [|"Low";"Medium";"High"|] 
    let phenotypes = [|"None";"Primary";"Secondary";"Teritary"|]
    let states = ctx.MkEnumSort("States",activity)
    let fates = ctx.MkEnumSort("Fates",phenotypes)
    let stateMap = Array.mapi (fun i item -> (item,i)) activity |> Map.ofArray
    let s = ctx.MkSolver()
    init ctx s states 0 states.Consts.[0] fates fates.Consts.[0]
    for i = 1 to bound do 
        step ctx s states fates (i-1) i
    match s.Check() with 
    | Status.SATISFIABLE -> printf "Sat\n"
                            for i = 0 to bound do
                                printf "Time: %O\nMAPK-L :\t%O\tMAPK-R :\t%O\n" (s.Model.Eval((ctx.MkIntConst(sprintf "Clock-%d" i )),true)) (s.Model.Eval(geneCreate ctx "MAPK" i 0 states,true)) (s.Model.Eval(geneCreate ctx "MAPK" i 1 states,true))
                                printf "Notch-L:\t%O\tNotch-R:\t%O\n" (s.Model.Eval(geneCreate ctx "Notch" i 0 states,true)) (s.Model.Eval(geneCreate ctx "Notch" i 1 states,true))
                                printf "Fate-L :\t%O\tFate-R :\t%O\n" (s.Model.Eval(geneCreate ctx "Fate" i 0 fates,true)) (s.Model.Eval(geneCreate ctx "Fate" i 1 fates,true))

    | Status.UNSATISFIABLE -> printf "Unsat\n"
    | _ -> failwith "Unknown\n"
