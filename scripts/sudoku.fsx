// This z3 needs to be a x86 dll
#load "getZ3.fsx"

#r "../platform/z3/bin/Microsoft.Z3.dll"

open Microsoft.Z3 

//
// Convenience wrappers for Z3 
//

// int sort
let mk_int_var (z3:Context) (name:string) = 
    z3.MkIntConst(name)

let mk_int (z3:Context) (i:int) = 
    z3.MkInt(i)

// arith expr
let mk_ge (z3:Context) (l:ArithExpr) (r:ArithExpr) = 
    z3.MkGe (l,r)

let mk_le (z3:Context) (l:ArithExpr) (r:ArithExpr) = 
    z3.MkLe (l,r)

// bool expr 
let mk_eq (z3:Context) (l:IntExpr) (r:IntExpr) = 
    z3.MkEq(l,r)

let mk_true (z3:Context) = 
    z3.MkTrue ()

let mk_and (z3:Context) (l:BoolExpr) (r:BoolExpr) = 
    z3.MkAnd ([|l;r|])

let mk_ands (z3:Context) (tt:BoolExpr list) = 
    z3.MkAnd(List.toArray tt)

// m <= x <= n
let m_le_x_le_n (z3:Context) (m:ArithExpr) (x:ArithExpr) (n:ArithExpr) = 
    mk_and z3 (mk_le z3 m x) (mk_le z3 x n)

let mk_distinct (z3:Context) (tt:Expr list) = 
    z3.MkDistinct (List.toArray tt)


// 
// The sudoku grid (mutable 2D array) and the constraints on it.
//
type sudoku_grid = IntExpr[,]

// grid ctor
let mk_grid ctx = 
    let x = Array2D.init 9 9 (fun i j -> mk_int_var ctx ("x"+(string)i+(string)j)) 
    x

// grid init
open System.Collections.Generic

let init_grid (ctx:Context) (x:IntExpr[,]) (s:string) = 

    let parse (s:string)  = 
        let ht = ref []
        let lines = s.Split([|'\n'|])
        assert(lines.Length = 9)
        for row=0 to 8 do
            let items = lines.[row].ToCharArray ()
            assert(items.Length = 9)
            for col = 0 to 8 do 
                match items.[col] with
                | '-' -> ()
                | x -> 
                    let i = (int)x - (int)'0'
                    ht := (row,col,i) :: !ht
        !ht

    let items = parse s
    let eqs = List.map
                (fun (row,col,data) -> mk_eq ctx x.[row,col] (mk_int ctx data))
                items
    mk_ands ctx eqs

// 1 <= x_{i,j} <= 9
let range ctx (x:IntExpr[,]) =        
    let r x = m_le_x_le_n ctx (mk_int ctx 1) x (mk_int ctx 9)
    mk_ands ctx [ r x.[0,0]; r x.[1,0]; r x.[2,0]; r x.[3,0]; r x.[4,0]; r x.[5,0]; r x.[6,0]; r x.[7,0]; r x.[8,0];
                  r x.[0,1]; r x.[1,1]; r x.[2,1]; r x.[3,1]; r x.[4,1]; r x.[5,1]; r x.[6,1]; r x.[7,1]; r x.[8,1];
                  r x.[0,2]; r x.[1,2]; r x.[2,2]; r x.[3,2]; r x.[4,2]; r x.[5,2]; r x.[6,2]; r x.[7,2]; r x.[8,2];
                  r x.[0,3]; r x.[1,3]; r x.[2,3]; r x.[3,3]; r x.[4,3]; r x.[5,3]; r x.[6,3]; r x.[7,3]; r x.[8,3];
                  r x.[0,4]; r x.[1,4]; r x.[2,4]; r x.[3,4]; r x.[4,4]; r x.[5,4]; r x.[6,4]; r x.[7,4]; r x.[8,4];
                  r x.[0,5]; r x.[1,5]; r x.[2,5]; r x.[3,5]; r x.[4,5]; r x.[5,5]; r x.[6,5]; r x.[7,5]; r x.[8,5];
                  r x.[0,6]; r x.[1,6]; r x.[2,6]; r x.[3,6]; r x.[4,6]; r x.[5,6]; r x.[6,6]; r x.[7,6]; r x.[8,6];
                  r x.[0,7]; r x.[1,7]; r x.[2,7]; r x.[3,7]; r x.[4,7]; r x.[5,7]; r x.[6,7]; r x.[7,7]; r x.[8,7];
                  r x.[0,8]; r x.[1,8]; r x.[2,8]; r x.[3,8]; r x.[4,8]; r x.[5,8]; r x.[6,8]; r x.[7,8]; r x.[8,8] ]

// x00 # x10 # ... # x80
let col_distinct ctx (x:IntExpr[,]) = 
    let a = [x.[0,0]; x.[1,0]; x.[2,0]; x.[3,0]; x.[4,0]; x.[5,0]; x.[6,0]; x.[7,0]; x.[8,0]]
    //Why does the below pass the type checker but substituting the above variable "a" not?
    mk_ands ctx [ (mk_distinct ctx [x.[0,0]; x.[1,0]; x.[2,0]; x.[3,0]; x.[4,0]; x.[5,0]; x.[6,0]; x.[7,0]; x.[8,0]]);
                  (mk_distinct ctx [x.[0,1]; x.[1,1]; x.[2,1]; x.[3,1]; x.[4,1]; x.[5,1]; x.[6,1]; x.[7,1]; x.[8,1]]);
                  (mk_distinct ctx [x.[0,2]; x.[1,2]; x.[2,2]; x.[3,2]; x.[4,2]; x.[5,2]; x.[6,2]; x.[7,2]; x.[8,2]]);
                  (mk_distinct ctx [x.[0,3]; x.[1,3]; x.[2,3]; x.[3,3]; x.[4,3]; x.[5,3]; x.[6,3]; x.[7,3]; x.[8,3]]);
                  (mk_distinct ctx [x.[0,4]; x.[1,4]; x.[2,4]; x.[3,4]; x.[4,4]; x.[5,4]; x.[6,4]; x.[7,4]; x.[8,4]]);
                  (mk_distinct ctx [x.[0,5]; x.[1,5]; x.[2,5]; x.[3,5]; x.[4,5]; x.[5,5]; x.[6,5]; x.[7,5]; x.[8,5]]);
                  (mk_distinct ctx [x.[0,6]; x.[1,6]; x.[2,6]; x.[3,6]; x.[4,6]; x.[5,6]; x.[6,6]; x.[7,6]; x.[8,6]]);
                  (mk_distinct ctx [x.[0,7]; x.[1,7]; x.[2,7]; x.[3,7]; x.[4,7]; x.[5,7]; x.[6,7]; x.[7,7]; x.[8,7]]);
                  (mk_distinct ctx [x.[0,8]; x.[1,8]; x.[2,8]; x.[3,8]; x.[4,8]; x.[5,8]; x.[6,8]; x.[7,8]; x.[8,8]]) ]

// x00 # x01 # ... # x08
let row_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[0,0]; x.[0,1]; x.[0,2]; x.[0,3]; x.[0,4]; x.[0,5]; x.[0,6]; x.[0,7]; x.[0,8]]);
                  (mk_distinct ctx [x.[1,0]; x.[1,1]; x.[1,2]; x.[1,3]; x.[1,4]; x.[1,5]; x.[1,6]; x.[1,7]; x.[1,8]]);
                  (mk_distinct ctx [x.[2,0]; x.[2,1]; x.[2,2]; x.[2,3]; x.[2,4]; x.[2,5]; x.[2,6]; x.[2,7]; x.[2,8]]);
                  (mk_distinct ctx [x.[3,0]; x.[3,1]; x.[3,2]; x.[3,3]; x.[3,4]; x.[3,5]; x.[3,6]; x.[3,7]; x.[3,8]]);
                  (mk_distinct ctx [x.[4,0]; x.[4,1]; x.[4,2]; x.[4,3]; x.[4,4]; x.[4,5]; x.[4,6]; x.[4,7]; x.[4,8]]);
                  (mk_distinct ctx [x.[5,0]; x.[5,1]; x.[5,2]; x.[5,3]; x.[5,4]; x.[5,5]; x.[5,6]; x.[5,7]; x.[5,8]]);
                  (mk_distinct ctx [x.[6,0]; x.[6,1]; x.[6,2]; x.[6,3]; x.[6,4]; x.[6,5]; x.[6,6]; x.[6,7]; x.[6,8]]);
                  (mk_distinct ctx [x.[7,0]; x.[7,1]; x.[7,2]; x.[7,3]; x.[7,4]; x.[7,5]; x.[7,6]; x.[7,7]; x.[7,8]]);
                  (mk_distinct ctx [x.[8,0]; x.[8,1]; x.[8,2]; x.[8,3]; x.[8,4]; x.[8,5]; x.[8,6]; x.[8,7]; x.[8,8]]) ]

let subgrid_distinct ctx (x:IntExpr[,]) = 
    mk_ands ctx [ (mk_distinct ctx [x.[0,0]; x.[0,1]; x.[0,2];    //X--
                                    x.[1,0]; x.[1,1]; x.[1,2];    //---
                                    x.[2,0]; x.[2,1]; x.[2,2];]); //---
                  (mk_distinct ctx [x.[3,0]; x.[3,1]; x.[3,2];    //---
                                    x.[4,0]; x.[4,1]; x.[4,2];    //X--
                                    x.[5,0]; x.[5,1]; x.[5,2];]); //---
                  (mk_distinct ctx [x.[6,0]; x.[6,1]; x.[6,2];    //---
                                    x.[7,0]; x.[7,1]; x.[7,2];    //---
                                    x.[8,0]; x.[8,1]; x.[8,2];]); //X--
                //
                  (mk_distinct ctx [x.[0,3]; x.[0,4]; x.[0,5];    //-X-
                                    x.[1,3]; x.[1,4]; x.[1,5];
                                    x.[2,3]; x.[2,4]; x.[2,5];]); 
                  (mk_distinct ctx [x.[3,3]; x.[3,4]; x.[3,5];
                                    x.[4,3]; x.[4,4]; x.[4,5];
                                    x.[5,3]; x.[5,4]; x.[5,5];]); 
                  (mk_distinct ctx [x.[6,3]; x.[6,4]; x.[6,5];
                                    x.[7,3]; x.[7,4]; x.[7,5];
                                    x.[8,3]; x.[8,4]; x.[8,5];]); 
                //      
                  (mk_distinct ctx [x.[0,6]; x.[0,7]; x.[0,8];    //--X
                                    x.[1,6]; x.[1,7]; x.[1,8];
                                    x.[2,6]; x.[2,7]; x.[2,8]]); 
                  (mk_distinct ctx [x.[3,6]; x.[3,7]; x.[3,8];
                                    x.[4,6]; x.[4,7]; x.[4,8];
                                    x.[5,6]; x.[5,7]; x.[5,8]]); 
                  (mk_distinct ctx [x.[6,6]; x.[6,7]; x.[6,8];
                                    x.[7,6]; x.[7,7]; x.[7,8];
                                    x.[8,6]; x.[8,7]; x.[8,8]]) ]

open System.Diagnostics

let main _ = 
    let ctx = new Context()
    // Guardian Sudoku #3,138 http://www.theguardian.com/lifeandstyle/2015/jun/02/sudoku-3138-medium. 
    let grid_data = "3--29--5-\n--2-4-693\n------8-4\n-8-------\n--9-7-3--\n-------4-\n5-8------\n274-6-5--\n-3--24--9"

    let g0 = mk_grid ctx 
    
    let solver = ctx.MkSolver()
    let init = init_grid ctx g0 grid_data

    let stopwatch = Stopwatch.StartNew()
    solver.Add (mk_ands ctx [init; range ctx g0; row_distinct ctx g0; col_distinct ctx g0; subgrid_distinct ctx g0])
    let r = solver.Check([||])
    stopwatch.Stop()
    Printf.printf "time %Oms\n" stopwatch.ElapsedMilliseconds

    match r with
    | Status.UNSATISFIABLE -> 
        Printf.printf "unsat"
    | Status.SATISFIABLE -> 
        Printf.printfn "Sat."                
        let m = solver.Model
        printf 
            "%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n%O %O %O %O %O %O %O %O %O\n" 
            (m.ConstInterp(g0.[0,0])) (m.ConstInterp(g0.[0,1])) (m.ConstInterp(g0.[0,2])) (m.ConstInterp(g0.[0,3])) (m.ConstInterp(g0.[0,4])) (m.ConstInterp(g0.[0,5])) (m.ConstInterp(g0.[0,6])) (m.ConstInterp(g0.[0,7])) (m.ConstInterp(g0.[0,8]))
            (m.ConstInterp(g0.[1,0])) (m.ConstInterp(g0.[1,1])) (m.ConstInterp(g0.[1,2])) (m.ConstInterp(g0.[1,3])) (m.ConstInterp(g0.[1,4])) (m.ConstInterp(g0.[1,5])) (m.ConstInterp(g0.[1,6])) (m.ConstInterp(g0.[1,7])) (m.ConstInterp(g0.[1,8]))
            (m.ConstInterp(g0.[2,0])) (m.ConstInterp(g0.[2,1])) (m.ConstInterp(g0.[2,2])) (m.ConstInterp(g0.[2,3])) (m.ConstInterp(g0.[2,4])) (m.ConstInterp(g0.[2,5])) (m.ConstInterp(g0.[2,6])) (m.ConstInterp(g0.[2,7])) (m.ConstInterp(g0.[2,8]))
            (m.ConstInterp(g0.[3,0])) (m.ConstInterp(g0.[3,1])) (m.ConstInterp(g0.[3,2])) (m.ConstInterp(g0.[3,3])) (m.ConstInterp(g0.[3,4])) (m.ConstInterp(g0.[3,5])) (m.ConstInterp(g0.[3,6])) (m.ConstInterp(g0.[3,7])) (m.ConstInterp(g0.[3,8]))
            (m.ConstInterp(g0.[4,0])) (m.ConstInterp(g0.[4,1])) (m.ConstInterp(g0.[4,2])) (m.ConstInterp(g0.[4,3])) (m.ConstInterp(g0.[4,4])) (m.ConstInterp(g0.[4,5])) (m.ConstInterp(g0.[4,6])) (m.ConstInterp(g0.[4,7])) (m.ConstInterp(g0.[4,8]))
            (m.ConstInterp(g0.[5,0])) (m.ConstInterp(g0.[5,1])) (m.ConstInterp(g0.[5,2])) (m.ConstInterp(g0.[5,3])) (m.ConstInterp(g0.[5,4])) (m.ConstInterp(g0.[5,5])) (m.ConstInterp(g0.[5,6])) (m.ConstInterp(g0.[5,7])) (m.ConstInterp(g0.[5,8]))
            (m.ConstInterp(g0.[6,0])) (m.ConstInterp(g0.[6,1])) (m.ConstInterp(g0.[6,2])) (m.ConstInterp(g0.[6,3])) (m.ConstInterp(g0.[6,4])) (m.ConstInterp(g0.[6,5])) (m.ConstInterp(g0.[6,6])) (m.ConstInterp(g0.[6,7])) (m.ConstInterp(g0.[6,8]))
            (m.ConstInterp(g0.[7,0])) (m.ConstInterp(g0.[7,1])) (m.ConstInterp(g0.[7,2])) (m.ConstInterp(g0.[7,3])) (m.ConstInterp(g0.[7,4])) (m.ConstInterp(g0.[7,5])) (m.ConstInterp(g0.[7,6])) (m.ConstInterp(g0.[7,7])) (m.ConstInterp(g0.[7,8]))
            (m.ConstInterp(g0.[8,0])) (m.ConstInterp(g0.[8,1])) (m.ConstInterp(g0.[8,2])) (m.ConstInterp(g0.[8,3])) (m.ConstInterp(g0.[8,4])) (m.ConstInterp(g0.[8,5])) (m.ConstInterp(g0.[8,6])) (m.ConstInterp(g0.[8,7])) (m.ConstInterp(g0.[8,8]))
        
    | Status.UNKNOWN -> 
        Printf.printf "unknown"
    | _ -> Printf.printf "x"
    
    ctx.Dispose ()

main ()
