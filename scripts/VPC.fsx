#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

//Simplified VPC model with only two cells
//Each cell has IS, LS, Notch, MAPK, LET60, Fate, Moved genes
//The activity of Notch is determined by the activity of LS *in the neighbouring cell*

type concurrency = Sync | Async | BoundedAsync 

let geneCreate (ctx:Context) name t position states =
    ctx.MkConst((sprintf "%s-%d-%d" name t position),states)

let moveMarker (ctx:Context) t position =
    ctx.MkBoolConst(sprintf "Move-%d-%d" t position)
let cellUpdate (ctx:Context) (states:EnumSort) (fates:EnumSort) t t' position altPosition isInput = 
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
    let lsN' = geneCreate ctx "LS" t' altPosition states
    
    //Bounded asynchrony markers
    // let move  = moveMarker ctx t  position
    // let move' = moveMarker ctx t' position
    // let moveUpdate = ctx.MkAnd(ctx.MkEq(move,ctx.MkFalse()),ctx.MkEq(move',ctx.MkTrue()))

    let let60Update = ctx.MkEq(let60',is) 
    let isUpdate = ctx.MkEq(is',isInput)
    let notchUpdate =   ctx.MkAnd([|
                                    ctx.MkImplies(ctx.MkNot(ctx.MkEq(mapk',states.Consts.[2])),ctx.MkEq(notch',lsN'))
                                    ctx.MkImplies((ctx.MkEq(mapk',states.Consts.[2])),ctx.MkEq(notch',states.Consts.[0])) |] )
    let lsUpdate = ctx.MkEq(ls',mapk')
    let mapkUpdate =   ctx.MkAnd( [|
                                    // //No notch inhibition
                                    // ctx.MkImplies(ctx.MkEq(let60,states.Consts.[0]),ctx.MkEq(mapk',states.Consts.[0]))
                                    // ctx.MkImplies(ctx.MkEq(let60,states.Consts.[1]),ctx.MkEq(mapk',states.Consts.[1]))
                                    // ctx.MkImplies(ctx.MkEq(let60,states.Consts.[2]),ctx.MkEq(mapk',states.Consts.[2]))
                                    ctx.MkImplies(ctx.MkEq(notch,states.Consts.[2]),ctx.MkEq(mapk',states.Consts.[0]))
                                    ctx.MkImplies(ctx.MkAnd(ctx.MkNot(ctx.MkEq(notch,states.Consts.[2])),ctx.MkEq(let60,states.Consts.[0])),ctx.MkEq(mapk',states.Consts.[0]))
                                    ctx.MkImplies(ctx.MkAnd(ctx.MkNot(ctx.MkEq(notch,states.Consts.[2])),ctx.MkEq(let60,states.Consts.[1])),ctx.MkEq(mapk',states.Consts.[1]))
                                    ctx.MkImplies(ctx.MkAnd(ctx.MkNot(ctx.MkEq(notch,states.Consts.[2])),ctx.MkEq(let60,states.Consts.[2])),ctx.MkEq(mapk',states.Consts.[2]))
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
                                                                ctx.MkGt(ctx.MkIntConst(sprintf "Clock-%d" t ),ctx.MkInt(10)),
                                                                ctx.MkNot(ctx.MkEq(mapk,states.Consts.[2])),
                                                                ctx.MkNot(ctx.MkEq(notch,states.Consts.[2]))
                                                                ),ctx.MkEq(fate',fates.Consts.[3]))
                                    ctx.MkImplies(  ctx.MkAnd(
                                                                ctx.MkNot(ctx.MkGt(ctx.MkIntConst(sprintf "Clock-%d" t ),ctx.MkInt(10))),
                                                                ctx.MkNot(ctx.MkEq(mapk,states.Consts.[2])),
                                                                ctx.MkNot(ctx.MkEq(notch,states.Consts.[2]))
                                                                ),ctx.MkEq(fate',fate))
                                    |])
    ctx.MkAnd([|let60Update;isUpdate;notchUpdate;lsUpdate;mapkUpdate;fateUpdate|])

let cellStatic (ctx:Context) t t' position altPosition states fates=
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

    let lsN' = geneCreate ctx "LS" t' altPosition states

    let fate = geneCreate ctx "Fate" t position fates
    let fate' = geneCreate ctx "Fate" t' position fates

    let notchUpdate = ctx.MkEq(notch',lsN')
    let isUpdate = ctx.MkEq(is,is')
    let let60Update = ctx.MkEq(let60,let60')
    let mapkUpdate = ctx.MkEq(mapk,mapk')
    let lsUpdate = ctx.MkEq(ls,ls')
    let fateUpdate = ctx.MkEq(fate,fate')

    ctx.MkAnd([|notchUpdate;isUpdate;let60Update;mapkUpdate;lsUpdate;fateUpdate|])

let step (ctx:Context) (s:Solver) (states:EnumSort) (fates:EnumSort) t t' c ac0 ac1 =
    let clock  = ctx.MkIntConst(sprintf "Clock-%d" t )
    let clock' = ctx.MkIntConst(sprintf "Clock-%d" t')
    let timer = ctx.MkEq(clock',ctx.MkAdd(clock,ctx.MkInt(1)))
    //Variables in cell 0
    let cell0Update = cellUpdate ctx states fates t t' 0 1 states.Consts.[ac0]
    let cell1Update = cellUpdate ctx states fates t t' 1 0 states.Consts.[ac1]
    let cell0Static = cellStatic ctx t t' 0 1 states fates
    let cell1Static = cellStatic ctx t t' 1 0 states fates
    //Bounded asynchrony
    let move0  = moveMarker ctx t  0
    let move1  = moveMarker ctx t  1
    let move0' = moveMarker ctx t' 0
    let move1' = moveMarker ctx t' 1
    let ignoreMove = ctx.MkAnd([|ctx.MkEq(move0,move1);ctx.MkEq(move0,move0');ctx.MkEq(move1,move1');ctx.MkEq(move0,ctx.MkFalse())|])
    let move0Constraint = ctx.MkAnd(ctx.MkEq(move0,ctx.MkFalse()),ctx.MkEq(move0',ctx.MkTrue()))
    let move1Constraint = ctx.MkAnd(ctx.MkEq(move1,ctx.MkFalse()),ctx.MkEq(move1',ctx.MkTrue()))
    let moveReset = ctx.MkAnd([|ctx.MkEq(move0,move1);ctx.MkEq(move0,ctx.MkTrue());ctx.MkEq(move0',move1');ctx.MkEq(move0',ctx.MkFalse())|])
    //ctx.MkAnd(cell0Update,cell1Update)
    //let systemUpdate = ctx.MkOr([|ctx.MkAnd(cell0Update,cell1Update);ctx.MkAnd(cell0Update,cell1Static);ctx.MkAnd(cell0Static,cell1Update);ctx.MkAnd([|moveReset;cell0Static;cell1Static|])|])
    //Synchronous
    //let systemUpdate = ctx.MkOr([|ctx.MkAnd(cell0Update,cell1Update);ctx.MkAnd([|moveReset;cell0Static;cell1Static|])|])
    //Asynchronous
    //let systemUpdate = ctx.MkOr([|ctx.MkAnd([|cell0Update;cell1Static;ctx.MkEq(move1,move1')|]);ctx.MkAnd([|cell0Static;cell1Update;ctx.MkEq(move0,move0')|]);ctx.MkAnd([|moveReset;cell0Static;cell1Static|])|])
    //Bounded asynchrony
    let systemUpdate = //ctx.MkOr([|ctx.MkAnd(cell0Update,cell1Update);ctx.MkAnd([|cell0Update;cell1Static;ctx.MkEq(move1,move1')|]);ctx.MkAnd([|cell0Static;cell1Update;ctx.MkEq(move0,move0')|]);ctx.MkAnd([|moveReset;cell0Static;cell1Static|])|])
                        match c with
                        //| Sync -> ctx.MkOr([|ctx.MkAnd(cell0Update,cell1Update);ctx.MkAnd([|moveReset;cell0Static;cell1Static|])|])
                        | Sync -> ctx.MkAnd([|cell0Update;cell1Update;ignoreMove|])
                        | Async ->ctx.MkAnd(ctx.MkXor(ctx.MkAnd(cell0Update,cell1Static),ctx.MkAnd(cell1Update,cell0Static)),ignoreMove)
                        | BoundedAsync ->ctx.MkOr([|ctx.MkAnd([|cell0Update;move0Constraint;cell1Update;move1Constraint|]);ctx.MkAnd([|cell0Update;move0Constraint;cell1Static;ctx.MkEq(move1,move1')|]);ctx.MkAnd([|cell0Static;cell1Update;move1Constraint;ctx.MkEq(move0,move0')|]);ctx.MkAnd([|moveReset;cell0Static;cell1Static|])|])
    s.Add(ctx.MkAnd([|timer;systemUpdate|]))

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

let simPrint (ctx:Context) (s:Solver) bound states fates =
    for i = 0 to bound do
        printf "Time: %O\n" (s.Model.Eval((ctx.MkIntConst(sprintf "Clock-%d" i )),true))
        printf "IS-L   :\t%O\tIS-R   :\t%O\n" (s.Model.Eval(geneCreate ctx "IS" i 0 states,true)) (s.Model.Eval(geneCreate ctx "IS" i 1 states,true))
        printf "LET60-L:\t%O\tLET60-R:\t%O\n" (s.Model.Eval(geneCreate ctx "LET60" i 0 states,true)) (s.Model.Eval(geneCreate ctx "LET60" i 1 states,true))
        printf "MAPK-L :\t%O\tMAPK-R :\t%O\n" (s.Model.Eval(geneCreate ctx "MAPK" i 0 states,true)) (s.Model.Eval(geneCreate ctx "MAPK" i 1 states,true))
        printf "Notch-L:\t%O\tNotch-R:\t%O\n" (s.Model.Eval(geneCreate ctx "Notch" i 0 states,true)) (s.Model.Eval(geneCreate ctx "Notch" i 1 states,true))
        printf "LS-L   :\t%O\tLS-R   :\t%O\n" (s.Model.Eval(geneCreate ctx "LS" i 0 states,true)) (s.Model.Eval(geneCreate ctx "LS" i 1 states,true))
        printf "Move-L :\t%O\tMove-R :\t%O\n" (s.Model.Eval(moveMarker ctx i 0)) (s.Model.Eval(moveMarker ctx i 1,true))
        printf "Fate-L :\t%O\tFate-R :\t%O\n" (s.Model.Eval(geneCreate ctx "Fate" i 0 fates,true)) (s.Model.Eval(geneCreate ctx "Fate" i 1 fates,true))

let main bound c verbose inputStates = 
    let ctx = new Context ()
    let genes = [|"IS";"LS";"Notch";"LET60";"MAPK";"Fate";"Moved"|]
    let activity = [|"Low";"Medium";"High"|] 
    let phenotypes = [|"None";"1'";"2'";"3'"|]
    let states = ctx.MkEnumSort("States",activity)
    let fates = ctx.MkEnumSort("Fates",phenotypes)
    let stateMap = Array.mapi (fun i item -> (item,i)) activity |> Map.ofArray
    let s = ctx.MkSolver()
    init ctx s states 0 states.Consts.[0] fates fates.Consts.[0]
    for i = 1 to bound do 
        step ctx s states fates (i-1) i c (Array.findIndex (fun e -> e=fst(inputStates)) activity) (Array.findIndex (fun e -> e=snd(inputStates)) activity)
    match s.Check() with 
    | Status.SATISFIABLE -> printf "Model sound (sat for unconstrained model up to bound %d). Now searching for fate patterns\n" bound
                            if verbose then simPrint ctx s bound states fates else ()
                            // s.Add(ctx.MkAnd([|ctx.MkEq(geneCreate ctx "Fate" bound 0 fates,fates.Consts.[3]);ctx.MkEq(geneCreate ctx "Fate" bound 1 fates,fates.Consts.[3])|]))
                            // match s.Check() with
                            // | Status.SATISFIABLE -> simPrint ctx s bound states fates
                            // | Status.UNSATISFIABLE -> printf "Unsat for fate"
                            // | _ -> failwith "Unknown result"
                            let endFates =  [   (fates.Consts.[1],fates.Consts.[1]);
                                                (fates.Consts.[1],fates.Consts.[2]);
                                                (fates.Consts.[1],fates.Consts.[3]);
                                                (fates.Consts.[2],fates.Consts.[1]);
                                                (fates.Consts.[2],fates.Consts.[2]);
                                                (fates.Consts.[2],fates.Consts.[3]);
                                                (fates.Consts.[3],fates.Consts.[1]);
                                                (fates.Consts.[3],fates.Consts.[2]);
                                                (fates.Consts.[3],fates.Consts.[3]);
                                                ]
                            for (i,j) in endFates do
                                s.Push()
                                s.Add(ctx.MkAnd([|ctx.MkEq(geneCreate ctx "Fate" bound 0 fates,i);ctx.MkEq(geneCreate ctx "Fate" bound 1 fates,j)|]))
                                match s.Check() with
                                | Status.SATISFIABLE -> 
                                    printf "Sat for fate %O %O\n" i j  
                                    if verbose then simPrint ctx s bound states fates else ()
                                    s.Pop() //simPrint ctx s bound states fates
                                | Status.UNSATISFIABLE -> 
                                    //printf "Unsat for fate %O %O\n" i j
                                    s.Pop()
                                | _ -> failwith "Unknown result"
                            printf "Complete\n"
    | Status.UNSATISFIABLE -> printf "Unsat\n"
    | _ -> failwith "Unknown\n"

printf "Testing synchronous model\n"
main 12 Sync false ("High","High")
printf "Testing asynchronous model\n"
main 12 Async false ("High","High")
printf "Testing bounded asynchronous model\n"
main 12 BoundedAsync false ("High","High")