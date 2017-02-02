#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

//You have a 5 litre jug, a 3 litre jug and as much water as you want. How do you get 4 litres of water exactly?
let assert_update (ctx:Context) (s:Solver) t t' = 
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
    
    //List all of the possible updates, turn them into constraints, add them to the solver
    let possibleUpdates = [|
                            ctx.MkAnd(doNothingFive,fillThree)
                            ctx.MkAnd(fillFive,doNothingThree)
                            ctx.MkAnd(doNothingFive,fillThree)
                            ctx.MkAnd(emptyFive,doNothingThree)
                            |]

    let constraints = ctx.MkOr(possibleUpdates)
    s.Add(constraints)
    ()

let step ctx s t t' =
    assert_update ctx s t t'
    assert_bounds ctx s

let main maxBound = 
    let rec core ctx =
        ()
    let ctx = new Context()

    //We have two variables, A and B, and they are each either 2 or 1, or both 2, or both 1
    // let variables = [|ctx.MkIntConst("A");ctx.MkIntConst("B")|]
    // let values = [|ctx.MkInt(2);ctx.MkInt(1)|]
    // let constraints = [|
    //     ctx.MkOr([|ctx.MkEq(variables.[0],values.[0]);ctx.MkEq(variables.[0],values.[1])|]);
    //     ctx.MkOr([|ctx.MkEq(variables.[1],values.[0]);ctx.MkEq(variables.[1],values.[1])|]);
    //     |]
    // let s = ctx.MkSolver()
    // s.Add(ctx.MkAnd(constraints))
    // //Must be satisfiable
    // printf "%s" (sanity_check ctx s variables)
    // //Paradox- must not be satisfiable
    // printf "%s" (paradox ctx s variables)
    // //A is less than B- What is the answer?
    // printf "%s" (answer ctx s variables)