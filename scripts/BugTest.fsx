#load "getZ3.fsx"

#r "../platform/z3/bin/Microsoft.Z3.dll"

open Microsoft.Z3 
let ctx = new Context()
let s = ctx.MkSolver()
let name = ctx.MkSymbol("Name")
let number = ctx.MkIntConst(name)

