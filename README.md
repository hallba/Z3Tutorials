# Using Z3 to solve logical and biological problems!

## Introduction

How do write timetable my day so that I meet everyone and avoid other peoples clashes? How do I get
4 litres of water exactly when I only have a 3 litre jug and a 5 litre jug? Can this Suduko puzzle 
even be solved? 

Each of these questions can arise in different ways for most people, sometimes in life or death 
situations if you are John McClane. These problems can be fun to solve, but most of the time you
just want an answer. 

In computer science these can be described as discrete models; where there either exists an answer
that satisfies all of the constraints or none exist. Tools called SAT and SMT solvers allow
you to describe the problem you are interested in and in an automated fashion will tell you if an answer
exists and what it is. They are used in CS to help find bugs in programs and hardware without running 
them as part of a class of techniques called *formal methods*.

These tools are not restricted to riddles and computer science, but can also be used to understand how
biological networks control cell development and differentiation. In the scientific literature such 
models are typically referred to as *executable* or *logical* models, but their underlying structures 
are discrete models. As such SAT and SMT solvers can be adapted and used to understand biological 
processes. One example of these in use is the BioModelAnalyzer (http://biomodelanalyzer.org, 
https://github.com/Microsoft/BioModelAnalyzer), where the SMT solver Z3 is used to search for simulations 
that have particular properties of interest.

In this tutorial we will show how Z3 can be used through the programming language F\# to solve different
problems. We restrict ourselves to specific models; a generic solution would require an input model 
format and an associated parser, which is outside of the scope of this demonstration.

### Getting started (tools and editors)

The specific tools you use will depend on the platform you use; some recommendations are below.

Regardless of the tools you use, your scripts will need a reference to where you have put the 
Z3 files. This is included in each script at the top where we specify a folder and a dll to use.

#### Windows

We recommend installing Visual Studio Code with the Ionide extension to write and run F\# scripts 
(http://ionide.io/). To install Z3, we suggest using the nuget package manager to install the latest 
Z3 package to the platform folder in your local repository.

## Timeless models

Here we give some examples of how to encode different constraints in Z3 to solve problems where we
are only interested in systems where elements don't change in time.

### Which number is bigger? Simple.fsx

In this example we will create an artificial problem to demonstrate different outputs from Z3. In this
problem we have two variables, called A and B. Each variable is equal to either 1 or 2, and we want to 
know whether there are specific values each can have. 

The main function contains the basic constraints that define what A and B must be. However, to demonstrate
the outputs from Z3 we have created three functions that include different final constraints. 

Firstly, we establish a *context* with the line

    let ctx = new Context()
	
You can think of a context as a palette of different constraints and types you can use to describe your
model. It provides the expressions you need to describe your model in an appropriate type for Z3. Typically 
programmers will wrap common functions for a context in dedicated functions to maintain
commmon F\# stylings, but in most of the tutorials here we use the context directly. In the Suduko example
below we show how we can wrap the functions.

We can then define elements using the context. We can define an integer constant with the name "A" as 

    let A = ctx.MkIntConst("A")
	
This tells Z3 that A is a constant integer; we also want to specify that it is either equal to one or two.
We need to specify these numbers through the context as below

	let zOne = ctx.MkInt(1)
	
We can now use these variables in F\# to make more complex expressions involving them. We know that A can be
one or two, so we need to specify that in through Z3

	ctx.MkOr([|ctx.MkEq(A,zOne);ctx.MkEq(A,zTwo)|])
	
This line states that A=1 using MkEq, or A=2 using the variables we have previously defined. We put this term, and another 
term referencing B into an array called constraints.

These constraints define the problem, but we need to tell Z3 to use them. To do this we need a *Solver*, to which
we add the constraints before asking for a solution. We create a solver, then add that all constraints must
in the array must be respected by using an "And" term

    let s = ctx.MkSolver()
    s.Add(ctx.MkAnd(constraints))
	
Now we can start testing! Three functions run different tests- 

* *sanity_check* doesn't add anything but shows that the model is sound
* *paradox* adds constraints that cannot be satisfied and should show that
* *answer* addresses a "real" question- if A is less than B, is there a solution?

In each case we copy the solver, test the model, and then throw away the results. This allows us to add
different, contradictory constraints without altering the other questions. To do this, we run three commands

	s.Push()
	
Copies the system. Any new constraints you add will only be added to the copy, and any tests you perform
will only be done on the working copy.

	s.Check()
	
Asks Z3 to return an answer. The result can be tested using pattern matching as satisfiable (there is a
solution), unsatisfiable (there is no solution) or unknown (something went wrong).

	s.Pop()
	
Discards the working copy and reverts to the original copy.

Looking in *sanity_check* we copy the model, and test without adding any additional constraints. We then test
the result and find that it is satisfiable. We then draw out the solution using 

	s.Model.ConstInterp
	
We can then print this to the terminal before discarding the result.

The function *paradox* is similar, but we add an additional, impossible to satisfy constraint

	ctx.MkAnd([|ctx.MkEq(variables.[0],ctx.MkInt(2));ctx.MkEq(variables.[0],ctx.MkInt(1))|])
	
This says that A is equal to 2 and A is equal to 1. We then test, and should find that the result is 
unsatisfiable.

Finally the function *answer* adds a constraint that specifies that A is less than B. We can then retest
and find the solution. MkLt is the expression used, meaning make less than (i.e. <), and there are other
functions like MkLe (make less than or equal), MkGt, and MkGe.

	ctx.MkLt(variables.[0],variables.[1])

Note that because we have used Push() and Pop() in each function, the constraints
in paradox and answer functions do not clash; in each case we have only added them to a working copy that
is discarded at the end of the work. 

We do a further test in answer; we look at the solution that Check() gives us, and ask for an alternative
answer. This is done by adding a further constraint to the solver, excluding the observed result from the 
solver, and running Check() again.

### Liars paradox: Liars.fsx

Lots of useful things can be described using integers, but they aren't the only types available in Z3. 
This script shows you how to use Enumerated Sorts (where you can define a set of states using strings),
and Boolean variables (i.e. true or false).

The liars paradox is a statement that is self contradictory; a Cretan says "All cretans are liars". 
This act of vile defamation notwithstanding, it cannot be correct- if the speaker is a Cretan, they
must be liar, therefore the statement is untrue (which in turn makes it true...). Here we just use it
as an example of how to use different types in Z3.

We can define "Nationalities" using the enumerated sort

    let nationalities = ctx.MkEnumSort("Nationalities",[|"Cretans";"EveryoneElse"|])

We can then reference this when creating new constants

    let speakNationality = ctx.MkConst("Nationality",nationalities)

We do this for both the nationalities, and the honesty of the speaker. 

Finally, after defining the variables, we create the constraints- if the speaker is Cretan, the 
speaker is a liar, and if the speaker is a liar, then the statement is false. We can add "if/then"
constraints using MkImplies

	ctx.MkImplies(ctx.MkEq(speakNationality,cretan),ctx.MkEq(speaker,falsehood))

We then specify that the speaker is Cretan ("paradox"), and add both the statement and the 
paradox to the solver to be checked. As written it is unsatisfiable, but you can leave out
constraints to create a satisfying model.

### Sudoko solver; sudoku.fsx & sudoku-bv.fsx

*Originally by @sishtiaq*

In the first "real world" problem, we will write a sudoku solver. In this example we do two different
things; we use the function MkDistinct(), which forces each element to be different, and we will explore
two ways to solve the problem; the first using integers, and the second using bitvectors. We will also 
wrap some of the Z3 functions so that they can be called in a more F\#-idiomatic way. 

If you're not familiar with Sudoku, you start a 9x9 square grid. Initially some numbers are filled in
but most are left blank. The game is to fill in the remaining blank spaces with numbers between 1 and
9. The constraint is that in each row, column, and 3x3 quadrant every number must only appear once. There 
should only be a single solution for a Sudoko solver.

First we create a 2D array of integer expressions and name them x-A-B where A and B are their indices
in the grid (*mk_grid*), and then read in an example puzzle taken from the Guardian (*init_grid*). Note
that throughout the code we use the same functions in the context as before but through locally 
defined functions. This can make the F\# code appear more natural e.g.

	let mk_int_var (z3:Context) (name:string) = 
	    z3.MkIntConst(name)

It also allows us to wrap slightly more complex expressions, for example to specify a range, 1 <= x <= 9
we can use the MkLe() 

	let mk_le (z3:Context) (l:ArithExpr) (r:ArithExpr) = 
	    z3.MkLe (l,r)
	let m_le_x_le_n (z3:Context) (m:ArithExpr) (x:ArithExpr) (n:ArithExpr) = 
	    mk_and z3 (mk_le z3 m x) (mk_le z3 x n)

We then set up constraints that specify the rules of the game. Firstly, *range* specifies that each 
element in the grid must be between 1 and 9 (using MkAnd() and MkLe() as above). We then need to 
specify that all elements in a row, column and quadrant are different. In each of these cases 
we use an expression MkDistinct() that specifies each element should be different.

mk_distinct ctx [x.[0,0]; x.[1,0]; x.[2,0]; x.[3,0]; x.[4,0]; x.[5,0]; x.[6,0]; x.[7,0]; x.[8,0]]

### Einsteins Riddle

## Ordering and simulations (bounded model checking)

### Die Hard with a Vengance

## Biological systems

### Proving stability of a Boolean network motif

### Synchrony, asynchrony and bounded asynchrony in vulval precursor development