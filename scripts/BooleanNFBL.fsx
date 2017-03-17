#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

//Prove stability in a negative feedforward loop (Perfect adaption motif)
// Input -> A
// A -> Output 
// Output -> B
// B -| Output
let makeVariable (ctx:Context) name time =
    ctx.MkBoolConst(sprintf "%s-%d" name time)

let issAllFP (ctx:Context) t t' =
    let input' = ctx.MkEq((makeVariable ctx "Input" t'),(makeVariable ctx "Input" t))
    let a' = ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "A" t))
    let b' = ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "B" t))
    let output' = ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "Output" t))
    ctx.MkAnd([|input';a';b';output'|])

let issAllFPEdgeCount (ctx:Context) t = 
    ctx.MkInt(0)

let issHalfFP (ctx:Context) t t' =
    let input' = ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkTrue())
    let a' = ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "A" t))
    let b' = ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "B" t))
    let output' = ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "Output" t))
    ctx.MkAnd([|input';a';b';output'|])

let issHalfFPEdgeCount (ctx:Context) t = 
    ctx.MkITE(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkTrue()),ctx.MkInt(0),ctx.MkInt(1))

let iss2Loop (ctx:Context) t t' = 
    let input' = ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkFalse())
    let a' = ctx.MkEq((makeVariable ctx "A" t'),ctx.MkFalse())
    let b' = ctx.MkEq((makeVariable ctx "B" t'),ctx.MkFalse())
    let output' = ctx.MkAnd([|
                                ctx.MkImplies((ctx.MkEq((makeVariable ctx "Output" t),ctx.MkFalse())),
                                                ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkTrue()));
                                ctx.MkImplies(ctx.MkEq((makeVariable ctx "Output" t),ctx.MkTrue()),
                                                ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkFalse()))
                    |])
    ctx.MkAnd([|input';a';b';output'|])

let iss2LoopEdgeCount (ctx:Context) t =
    let input' = ctx.MkITE(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkTrue()),ctx.MkInt(1),ctx.MkInt(0)) :?> IntExpr
    let a' = ctx.MkITE(ctx.MkEq((makeVariable ctx "A" t),ctx.MkTrue()),ctx.MkInt(1),ctx.MkInt(0)) :?> IntExpr
    let b' = ctx.MkITE(ctx.MkEq((makeVariable ctx "B" t),ctx.MkTrue()),ctx.MkInt(1),ctx.MkInt(0)) :?> IntExpr
    let output' = ctx.MkInt(1)
    
    ctx.MkAdd(input',a',b',output') 

let iss4Loop (ctx:Context) t t' =
    let input' = ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkFalse())
    let a' = ctx.MkAnd([|
                                ctx.MkImplies((ctx.MkEq((makeVariable ctx "Output" t),ctx.MkFalse())),
                                                ctx.MkEq((makeVariable ctx "A" t),(makeVariable ctx "A" t')));
                                ctx.MkImplies(ctx.MkEq((makeVariable ctx "Output" t),ctx.MkTrue()),
                                                ctx.MkEq((makeVariable ctx "A" t),ctx.MkNot(makeVariable ctx "A" t')))
                    |])
    let b' = ctx.MkEq((makeVariable ctx "B" t'),ctx.MkFalse())
    let output' = ctx.MkAnd([|
                                ctx.MkImplies((ctx.MkEq((makeVariable ctx "Output" t),ctx.MkFalse())),
                                                ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkTrue()));
                                ctx.MkImplies(ctx.MkEq((makeVariable ctx "Output" t),ctx.MkTrue()),
                                                ctx.MkEq((makeVariable ctx "Output" t'),ctx.MkFalse()))
                    |])
    ctx.MkAnd([|input';a';b';output'|])

let iss4LoopEdgeCount (ctx:Context) t =
    //This doesn't make sense really; this model is deterministic so there is always one out edge
    let input' = ctx.MkITE(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkTrue()),ctx.MkInt(1),ctx.MkInt(0)) :?> IntExpr
    let b' = ctx.MkITE(ctx.MkEq((makeVariable ctx "B" t),ctx.MkTrue()),ctx.MkInt(1),ctx.MkInt(0)) :?> IntExpr

    let a' = ctx.MkITE((ctx.MkEq((makeVariable ctx "Output" t),ctx.MkFalse())),ctx.MkInt(0),ctx.MkInt(1)) :?>IntExpr
    let output' = ctx.MkInt(1)
    //ctx.MkAdd(input',a',b',output')
    ctx.MkInt(1) 

let issNFB inputState (ctx:Context) t t' = 
    //Update input 
    let input' = if inputState then ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkTrue()) else ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkFalse())
    //Update A 
    let output' = ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "A" t))
    //Update B
    let b' = ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "Output" t))
    //Update Output
    let a' =   ctx.MkAnd(  [|
                                ctx.MkImplies(ctx.MkEq((makeVariable ctx "Input" t),(ctx.MkFalse())),ctx.MkEq((makeVariable ctx "A" t'),ctx.MkFalse()))
                                ctx.MkImplies(ctx.MkAnd(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkTrue()),ctx.MkEq((makeVariable ctx "B" t),(ctx.MkFalse()))),ctx.MkEq((makeVariable ctx "A" t'),ctx.MkTrue()))
                                ctx.MkImplies(ctx.MkAnd(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkTrue()),ctx.MkEq((makeVariable ctx "B" t),(ctx.MkTrue()))),ctx.MkEq((makeVariable ctx "A" t'),ctx.MkFalse()))
                                |]  )
    let a0 = ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "A" t))
    let b0 = ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "B" t))
    let input0 = ctx.MkEq((makeVariable ctx "Input" t'),(makeVariable ctx "Input" t))
    let output0 = ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "Output" t))
    
    let asyncChange = ctx.MkOr([|   
                                    ctx.MkAnd([|input'; a0; b0; output0|])
                                    ctx.MkAnd([|input0; a'; b0; output0|])
                                    ctx.MkAnd([|input0; a0; b'; output0|])
                                    ctx.MkAnd([|input0; a0; b0; output'|])
                                    |])
    //Something must change if it can
    let fairness = ctx.MkOr([| 
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "A" t)))
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "B" t)))
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "Output" t)))
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "Input" t'),(makeVariable ctx "Input" t)))
    |])
    ctx.MkAnd([|asyncChange;fairness|])

let issNFBEdgeCount inputState (ctx:Context) t =
    let input' =    if inputState then 
                        ctx.MkITE(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkFalse()),ctx.MkInt(1),ctx.MkInt(0) ) :?> IntExpr
                    else 
                        ctx.MkITE(ctx.MkEq((makeVariable ctx "Input" t),ctx.MkTrue()),ctx.MkInt(1),ctx.MkInt(0) ) :?> IntExpr
    let output' = ctx.MkITE(ctx.MkEq((makeVariable ctx "Output" t),(makeVariable ctx "A" t)),ctx.MkInt(0),ctx.MkInt(1)) :?> IntExpr
    let b' = ctx.MkITE(ctx.MkEq((makeVariable ctx "Output" t),(makeVariable ctx "B" t)),ctx.MkInt(0),ctx.MkInt(1)) :?> IntExpr
    let a' = ctx.MkITE(
                        ctx.MkOr(
                                    ctx.MkAnd(
                                            ctx.MkEq((makeVariable ctx "A" t),ctx.MkTrue()),
                                            ctx.MkEq((makeVariable ctx "Input" t),(ctx.MkFalse()))
                                            ),
                                    ctx.MkAnd(
                                            ctx.MkEq((makeVariable ctx "A" t),ctx.MkTrue()),
                                            ctx.MkEq((makeVariable ctx "B" t),ctx.MkTrue()),
                                            ctx.MkEq((makeVariable ctx "Input" t),(ctx.MkTrue()))
                                            ),
                                    ctx.MkAnd(
                                            ctx.MkEq((makeVariable ctx "A" t),ctx.MkFalse()),
                                            ctx.MkEq((makeVariable ctx "B" t),ctx.MkFalse()),
                                            ctx.MkEq((makeVariable ctx "Input" t),(ctx.MkTrue()))
                                            )
                                ),
                        ctx.MkInt(1),
                        ctx.MkInt(0)) :?> IntExpr
    ctx.MkAdd(input',output',b',a')

let issNFFEdgeCount inputState (ctx:Context) t =
    //Placeholder
    ctx.MkInt(0)
let issNFF inputState (ctx:Context) t t' = 
    //Update input 
    let input' = if inputState then ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkTrue()) else ctx.MkEq((makeVariable ctx "Input" t'),ctx.MkFalse())
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
    let a0 = ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "A" t))
    let b0 = ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "B" t))
    let input0 = ctx.MkEq((makeVariable ctx "Input" t'),(makeVariable ctx "Input" t))
    let output0 = ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "Output" t))
    
    let asyncChange = ctx.MkOr([|   
                                    ctx.MkAnd([|input'; a0; b0; output0|])
                                    ctx.MkAnd([|input0; a'; b0; output0|])
                                    ctx.MkAnd([|input0; a0; b'; output0|])
                                    ctx.MkAnd([|input0; a0; b0; output'|])
                                    |])
    //Something must change if it can
    let fairness = ctx.MkOr([| 
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "A" t'),(makeVariable ctx "A" t)))
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "B" t'),(makeVariable ctx "B" t)))
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "Output" t'),(makeVariable ctx "Output" t)))
                                ctx.MkNot(ctx.MkEq((makeVariable ctx "Input" t'),(makeVariable ctx "Input" t)))
    |])
    ctx.MkAnd([|asyncChange;fairness|])
let excludeState (ctx:Context) (s:Solver) t =
    s.Add(ctx.MkNot(ctx.MkAnd(
        [|
        ctx.MkEq(s.Model.Eval(makeVariable ctx "Input" t),makeVariable ctx "Input" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "A" t),makeVariable ctx "A" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "B" t),makeVariable ctx "B" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "Output" t),makeVariable ctx "Output" t)
        |]
    )))

let specifyModelState (ctx:Context) (s:Solver) t =
    s.Add(ctx.MkAnd(
        [|
        ctx.MkEq(s.Model.Eval(makeVariable ctx "Input" t),makeVariable ctx "Input" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "A" t),makeVariable ctx "A" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "B" t),makeVariable ctx "B" t)
        ctx.MkEq(s.Model.Eval(makeVariable ctx "Output" t),makeVariable ctx "Output" t)
        |]
    ))

type result = Bifurcation | NoBifurcation | SCC | Stable 

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


let makeSpecialVariable (ctx:Context) t name = 
    ctx.MkIntConst(sprintf "%s*%d*special" name t)
let makeLinkVariable ctx t =
    makeSpecialVariable ctx t "Link"
let makeSuccessorCountVariable ctx t =
    makeSpecialVariable ctx t "Successors"
let makeOutgoingEdgeCountVariable ctx t =
    makeSpecialVariable ctx t "OutgoingEdges"
let simPrint (ctx:Context) (s:Solver) length = 
    for t = 0 to length do  
        printf "T:\t%d\n" t
        printf "L:\t%O\n" (s.Model.Eval(makeLinkVariable ctx t))
        printf "S:\t%O\n" (s.Model.Eval(makeSuccessorCountVariable ctx t))
        printf "E:\t%O\n" (s.Model.Eval(makeOutgoingEdgeCountVariable ctx t))
        printf "I:\t%O\n" (s.Model.Eval(makeVariable ctx "Input" t))
        printf "A:\t%O\n" (s.Model.Eval(makeVariable ctx "A" t))
        printf "O:\t%O\n" (s.Model.Eval(makeVariable ctx "Output" t))
        printf "B:\t%O\n" (s.Model.Eval(makeVariable ctx "B" t))


let min (ctx:Context) (a:IntExpr) (b:IntExpr) = 
    ctx.MkITE(ctx.MkLt(a,b),a,b) :?> IntExpr
let minList (ctx:Context) (a:IntExpr list) =
    let rec core a acc = 
        match a with 
        | topExpr::rest -> core rest (min ctx acc topExpr)
        | [] -> acc
        
    match a with 
    | topExpr::rest -> core rest topExpr
    | [] -> failwith "Empty list"
let returnState (ctx:Context) (a:int) (b:int) step =
    ctx.MkITE((step ctx a b),(makeLinkVariable ctx b),ctx.MkInt(a)) :?> IntExpr
let findBSCC bound updateRule edgeCount =
    //Incomplete
    //Only finds a set or reachable states from "0"
    let ctx = new Context()
    let s = ctx.MkSolver()
    //Create the initial low-linker variable
    let linkerZero = makeLinkVariable ctx 0
    let initialLinkerState = ctx.MkEq(linkerZero,ctx.MkInt(0))
    printf "Searching for SCC at bound"
    s.Add(initialLinkerState)
    let rec core i bound =
        printf "...%d" i;
        //The last element should be a successor of some of the earlier ones
        let lastElementSuccessorofEarlier = ctx.MkOr(Array.init i (fun element -> updateRule ctx element i)) 
        //No element should be repeated
        let noRepeat = ctx.MkAnd(Array.init i (fun element -> ctx.MkNot(statesEqual ctx element i)) )
        s.Add(ctx.MkAnd([|lastElementSuccessorofEarlier;noRepeat|]))
        //Get out early if you can't find the states
        match s.Check() with 
        | Status.UNSATISFIABLE -> printf "\nNo traces longer than %d steps\n" (i-1); Stable
        | Status.SATISFIABLE ->
                                //Now specify the "back-linker" constraints following Tarjan
                                //Value of initial is 0
                                //the value of every middle one is the min(smallest reachable state,+1)
                                //the value of the end one is the linker value of the smallest reachable state
                                //Then we specify that they are all equal to 0 and add to S
                                
                                //This changes at each bound so we make a copy to work on
                                s.Push()
                                let lastLinkerValue site =  List.init (i+1) (fun element -> returnState ctx site element updateRule ) 
                                                            |> minList ctx
                                let laterLinkers = Array.init (i+1) (fun element -> ctx.MkEq(makeLinkVariable ctx (element+1),(lastLinkerValue (element+1)))) 
                                //Finally, specify that they are all equal to the first linker value i.e. its a scc (within the bound)
                                let allLinkBack= Array.init (i+1) (fun element -> ctx.MkEq((makeLinkVariable ctx (element+1)),ctx.MkInt(0)))

                                //For bottom
                                //Count number of successors
                                //Count number of outgoing edges 
                                //  (will need an alternative to update rule that returns int)
                                //For all states #successors==#outgoing edges
                                let numberSuccessors t t' =
                                    ctx.MkITE((updateRule ctx t t'),ctx.MkInt(1),ctx.MkInt(0)) :?> IntExpr
                                let rec countSuccessors t lastT acc =
                                    if lastT > -1 then 
                                        countSuccessors t (lastT-1) (ctx.MkAdd(acc,(numberSuccessors t lastT) )) 
                                    else acc
                                let successorCount =    Array.init (i+1) (fun t -> ((makeSuccessorCountVariable ctx t),t))
                                                        |> Array.map (fun (element,t) -> ctx.MkEq(element,(countSuccessors t i (ctx.MkInt(0))) ) )
                                
                                let outgoingCount = Array.init (i+1) (fun t -> ctx.MkEq((makeOutgoingEdgeCountVariable ctx t),(edgeCount ctx t) ) )
                                let successorsEqOutgoing = Array.init (i+1) (fun t -> ctx.MkEq(makeSuccessorCountVariable ctx t,makeOutgoingEdgeCountVariable ctx t))
                                s.Add(ctx.MkAnd[|    
                                                ctx.MkAnd(successorCount)
                                                ctx.MkAnd(outgoingCount)
                                                ctx.MkAnd(laterLinkers)
                                                ctx.MkAnd(allLinkBack)
                                                ctx.MkAnd(successorsEqOutgoing)
                                                |])
                                match s.Check() with 
                                | Status.SATISFIABLE -> printf "\nFound SCC with bound %d\n" i; simPrint ctx s i ; SCC
                                | Status.UNSATISFIABLE ->
                                    s.Pop()
                                    if i < bound then core (i+1) bound else printf "\n"; Stable
                                | _ -> failwith "Unknown result"
        | _ -> failwith "Unknown result"
    core 1 bound

let findStronglyConnectedSet bound step =
    let ctx = new Context()
    let s = ctx.MkSolver()
    printf "Searching for strongly connected set at bound"
    let rec core i bound =
        printf "...%d" i
        s.Add([|step ctx s (i-1) i|])
        //Check that there are traces this long; get out early if not
        match s.Check() with 
        | Status.UNSATISFIABLE -> printf "\nNo traces longer than %d steps\n" i; Stable
        | Status.SATISFIABLE -> 
                                s.Push()
                                //Specify that it is a loop
                                s.Add([|step ctx s i 0|])
                                match s.Check() with 
                                | Status.SATISFIABLE -> printf "\nFound cycle of length %d\n" i; simPrint ctx s i ; SCC
                                | Status.UNSATISFIABLE ->   s.Pop()
                                                            if i < bound then 
                                                                s.Add([|step ctx s i (i+1)|])
                                                                core (i+2) bound 
                                                            else printf "\n"; printf "\nStable within bound %d\n"bound; Stable
                                | _ -> failwith "Unknown result"
        | _ -> failwith "Unknown result"

    core 1 bound

let setFalse (ctx:Context) (s:Solver) t = 
    let a0 = ctx.MkEq((makeVariable ctx "A" t),ctx.MkFalse())
    let b0 = ctx.MkEq((makeVariable ctx "B" t),ctx.MkFalse())
    let input0 = ctx.MkEq((makeVariable ctx "Input" t),ctx.MkFalse())
    let output0 = ctx.MkEq((makeVariable ctx "Output" t),ctx.MkFalse())
    s.Add(ctx.MkAnd([|a0;b0;input0;output0|]))

let findTrace length updateRule =
    let ctx = new Context()
    let s = ctx.MkSolver()
    setFalse ctx s 0
    for i = 1 to length do
        s.Add([|updateRule ctx s (i-1) i|])
    match s.Check([||]) with
    | Status.SATISFIABLE -> printf "sat\n"
                            simPrint ctx s length
    | Status.UNSATISFIABLE -> printf "unsat\n"
    | _ -> failwith "x"

let main _ = 
    match findBSCC 16 (issNFB true) (issNFBEdgeCount true) with
            | SCC -> ()
            | Stable -> printf "Model has no BSCC!\n"
            | _ -> failwith "problem- error"
    