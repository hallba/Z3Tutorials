#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

//You have a 5 litre jug, a 3 litre jug and as much water as you want. How do you get 4 litres of water exactly?
let assertUpdate (ctx:Context) (s:Solver) t t' = 
    //Convienience integers
    let zZero = ctx.MkInt(0)
    let zFive = ctx.MkInt(5)
    let zThree = ctx.MkInt(3)

    //Create the variables
    let fiveState = ctx.MkIntConst(sprintf "Five-%d" t)
    let threeState = ctx.MkIntConst(sprintf "Three-%d" t)
    let fiveState' = ctx.MkIntConst(sprintf "Five-%d" t')
    let threeState' = ctx.MkIntConst(sprintf "Three-%d" t')

    //Simple updates; do nothing, fill from tap, empty to ground
    let doNothingFive = ctx.MkEq(fiveState,fiveState')
    let doNothingThree = ctx.MkEq(threeState,threeState')
    let fillFive = ctx.MkEq(fiveState',zFive)
    let fillThree = ctx.MkEq(threeState',zThree)
    let emptyFive = ctx.MkEq(fiveState',zZero)
    let emptyThree = ctx.MkEq(threeState',zZero)

    //Complex updates; fill three from five, fill five from three
    //Empty three into five (pair with emptyThree)
    let emptyThreeIntoFive = ctx.MkEq(fiveState',ctx.MkAdd(threeState,fiveState))
    //Fill five with some of three (pair with fillFive)
    let fillFivewithThree = ctx.MkEq(threeState',ctx.MkSub(threeState,ctx.MkSub(zFive,fiveState)))
    //Fill three with some of five (pair with fillThree)
    let fillThreewithFive = ctx.MkEq(fiveState',ctx.MkSub(fiveState,ctx.MkSub(zThree,threeState)))
    let topUpThree = ctx.MkEq(threeState',fiveState)

    //List all of the possible updates, turn them into constraints, add them to the solver
    let possibleUpdates = [|
                            ctx.MkAnd(doNothingFive,fillThree)
                            ctx.MkAnd(fillFive,doNothingThree)
                            ctx.MkAnd(doNothingFive,fillThree)
                            ctx.MkAnd(emptyFive,doNothingThree)
                            ctx.MkAnd(emptyThreeIntoFive,emptyThree)
                            ctx.MkAnd(fillFivewithThree,fillFive)
                            ctx.MkAnd(fillThreewithFive,ctx.MkOr(topUpThree,fillThree))
                            |]

    let constraints = ctx.MkOr(possibleUpdates)
    s.Add(constraints)

let assertBounds (ctx:Context) (s:Solver) t =
    //Convienience integers
    let zZero = ctx.MkInt(0)
    let zFive = ctx.MkInt(5)
    let zThree = ctx.MkInt(3)
    //Create the variables
    let fiveState = ctx.MkIntConst(sprintf "Five-%d" t)
    let threeState = ctx.MkIntConst(sprintf "Three-%d" t)

    let constraints = ctx.MkAnd([|
                                    ctx.MkGe(fiveState,zZero)
                                    ctx.MkLe(fiveState,zFive)
                                    ctx.MkGe(threeState,zZero)
                                    ctx.MkLe(threeState,zThree)
                                    |])
    s.Add(constraints)

let step ctx s t t' =
    assertUpdate ctx s t t'
    assertBounds ctx s t

let initial (ctx:Context) (s:Solver) t = 
    //Convienience integers
    let zZero = ctx.MkInt(0)
    //Create the variables
    let fiveState = ctx.MkIntConst(sprintf "Five-%d" t)
    let threeState = ctx.MkIntConst(sprintf "Three-%d" t)
    let constraints = ctx.MkAnd([|
                                    ctx.MkEq(fiveState,zZero)
                                    ctx.MkEq(threeState,zZero)
                                |])
    s.Add(constraints)

let final (ctx:Context) (s:Solver) t = 
    let zFour = ctx.MkInt(4)
    let fiveState = ctx.MkIntConst(sprintf "Five-%d" t)
    s.Add(ctx.MkEq(fiveState,zFour))
let main maxBound = 
    let ctx = new Context()
    let s = ctx.MkSolver()
    initial ctx s 0
    let rec core i =
        if i = maxBound then printf "No results within bound of %d\n" maxBound else 
            step ctx s (i-1) i
            s.Push()
            final ctx s i
            let r = s.Check([||])
            match r with 
            | Status.UNSATISFIABLE ->
                s.Pop()
                printf "Unsat- No answer with a bound of %d\n" i
                core (i+1)
            | Status.SATISFIABLE ->
                s.Pop()
                printf "Sat- Got a result at bound %d\n" i
                printf "3Jug\t5Jug\n"
                for t=0 to i do
                    let threeState = s.Model.ConstInterp(ctx.MkIntConst(sprintf "Three-%d" t))
                    let fiveState = s.Model.ConstInterp(ctx.MkIntConst(sprintf "Five-%d" t))
                    printf "%O\t%O\n" threeState fiveState
            | _ -> failwith "Unknown response from Z3"
    core 1