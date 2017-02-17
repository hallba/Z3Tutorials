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
should only be a single solution for a Sudoko solver. We will start with the integer version- sudoku.fsx

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
we use an expression MkDistinct() that specifies each element should be different. For example, in
an individual row

	mk_distinct ctx [x.[0,0]; x.[1,0]; x.[2,0]; x.[3,0]; x.[4,0]; x.[5,0]; x.[6,0]; x.[7,0]; x.[8,0]]
	
We can then add constraints for rows, columns, and quadrants and find a solution!

In the script sudoku-bv.fsx we use bitvectors instead of integers. This in principle can be faster
but this may depend on multiple different factors. Here the wrapping of functions in the previous
script makes the job easier; we can substitute bitvector equivalents for many of the functions. The 
most notable difference in the alternative forms is that we need to pass a bitvector size to the 
initialisation functions like MkBVConst; here we specify this value as a global unsigned integer.

### Einsteins Riddle

This example brings together some of the ideas from the previous examples. Here we have 5 people,
who live on the same street in different houses. Each house has a different colour, and each person
has a different drink, cigar, house colour, pet, and nationaility. From a list of facts (e.g. the 
Dane drinks tea) you need to work out where everyone lives and what they like.

As they all live on a street we can identify them by an integer, referencing their house number. 
We create integer constants for each preference, and set them to be distinct integers between one
and five.

We can then add equalities to represent each fact. The more complex constraints are those referencing
neighbours; here we need to use MkOr to specify that one or other is true. We then pass all of the
constraints to the solver and find the solution!

## Ordering and simulations (bounded model checking)

In each of the examples so far we have looked at systems that are effectively static in time. We
don't consider how entities change over time, just their relationships with one another. This is 
useful in some situations but, particularly in biology, we want to know how systems develop over 
time. Here we will start to look at some examples where we describe this type of system.

This can be referred to as bounded model checking; we look for solutions up to a bound (a number
of steps taken). Our solutions are therefore restricted to the bound; sometimes this is fine, but 
in others it can be a limitation. Increasing the bound makes the solutions slower and harder to 
find as the size of state space increases. In a worst case scenario an increase of a single step
may transform the problem from one that can be solved in ms to one that takes years! There may 
therefore be a largest theoretical bound that is greater than the bound that can practically be 
tested, and you should be aware that the results may not hold for larger, untestable bounds.

### Die Hard with a Vengance; DieHard.fsx

You have a 3 litre jug and a 5 litre jug, and need to measure 4 litres of water. You can empty 
the jugs onto the ground and into each other, and you can fill them from each other and the tap. 
Without measuring the volumes explicitly, how do you get 4 litres?

Now we have variables that change with time. The way we create them is not different from before
but we need to consider the initial state and the relationships between timepoints. 

In previous examples where variables did not change we created constants with the name of the 
variable; now we will add the time explictly to the variable name. So in the initial case we 
will have just two variables "Five-0" and "Three-0". Consistant naming is important so that we 
can encode the behaviour in a loop; other times will be written as "Five-%t" whtere %t is the time.

We then initialise the model, by specifying how full the jugs are initially. Each jug starts off empty
so we set the variables "Five-0" and "Three-0" to be equal to zero. 

We then define the transitions that the jugs can make according to the actions we can perform. 
This is done in the step function that asserts how the jugs update between two times, and the 
bounds of the jugs (i.e. the total amount of water they can hold). The update itself is specified
in assertUpdate. We can do a limited number of things;

* Empty each of the jugs
* Do nothing to each of the jugs
* Fill each of the jugs
* Transfer fluid from one jug to the other, leading to either one jug being filled or one emptied

In the last case the total volume of water must stay the same, and one of the jugs must be either
emptied or filled. We can then add all of the different options to an "Or" expression, and add this 
as a constraint. 

To test different bounds we then then use a loop and add a new step for each turn of the loop, testing
at each stage for a solution. If we run the main function with a maximum bound of 10 we can find a 
solution quickly, in only 6 steps! Now imagine that some updates are not allowed; for example, you couldn't 
empty the 3 litre jug without transferring the contents to the other jug. What happens to the solution?
Within a small bound, are there any update types you must have?

## Biological systems

### Proving stability of a Boolean network motif

Stability in a biological system is defined as the presence of a single fix point and no cycles (that is
loops with more than one state). It can be thought of as a measure of robustness; if you believe your
model represents a homoestatic system, or a system at equilibrium, this can 

### Synchrony, asynchrony and bounded asynchrony in vulval precursor development