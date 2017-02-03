#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 

// Who keeps the fish?
// 5 people, with different nationalities, different preferred drinks and cigars, and different pets live 
// in the same street. Each house has a different colour. Given these hints, who has the fish?
// From
// https://udel.edu/~os/riddle.html

let mk_distinct (z3:Context) (tt:Expr list) = 
    z3.MkDistinct (List.toArray tt)

let main () = 
    let ctx = new Context()
    let makeInt (item:string) = ctx.MkIntConst(item)
    let nationalities = [|"Briton";"German";"Swede";"Norwegian";"Dane"|] |> Array.map makeInt
    let houseColours = [|"Red";"Green";"White";"Yellow";"Blue"|] |> Array.map makeInt
    let drinks = [|"Tea";"Coffee";"Beer";"Water";"Milk"|] |> Array.map makeInt
    let smokes = [|"PallMall";"Dunhill";"BlueMaster";"Prince";"Blend"|] |> Array.map makeInt
    let pets = [|"Dogs";"Birds";"Cats";"Horses";"Fish"|] |> Array.map makeInt
    //For convienience, put each in an array
    let everything = [|nationalities;houseColours;drinks;smokes;pets|]
    //All live at house numbers from 1 to 5
    let range item = ctx.MkAnd(ctx.MkGe(item,ctx.MkInt(1)),ctx.MkLe(item,ctx.MkInt(5)))
    let streetRange = ctx.MkAnd(Array.collect (fun char -> Array.map range char) everything)
    //Make everything distinct
    let distinctNat = mk_distinct ctx [nationalities.[0];nationalities.[1];nationalities.[2];nationalities.[3];nationalities.[4]]
    let distinctCol = mk_distinct ctx [houseColours.[0];houseColours.[1];houseColours.[2];houseColours.[3];houseColours.[4]]
    let distinctDri = mk_distinct ctx [drinks.[0];drinks.[1];drinks.[2];drinks.[3];drinks.[4]]
    let distinctSmo = mk_distinct ctx [smokes.[0];smokes.[1];smokes.[2];smokes.[3];smokes.[4]]
    let distinctPet = mk_distinct ctx [pets.[0];pets.[1];pets.[2];pets.[3];pets.[4]]
    //Now look at the facts (NB houses read 1 to 5 left to right)
    //the Brit lives in the red house
    let britImpliesRed = ctx.MkEq(ctx.MkIntConst("Briton"),ctx.MkIntConst("Red"))
    //the Swede keeps dogs as pets
    let swedeImpliesDogs = ctx.MkEq(ctx.MkIntConst("Swede"),ctx.MkIntConst("Dogs"))
    //the Dane drinks tea
    let daneImpliesTea = ctx.MkEq(ctx.MkIntConst("Dane"),ctx.MkIntConst("Tea"))
    //the green house is on the left of the white house
    let greenImpliesWhiteMinusOne = ctx.MkEq(ctx.MkIntConst("White"),ctx.MkAdd(ctx.MkIntConst("Green"),ctx.MkInt(1)))
    //the green house's owner drinks coffee
    let greenImpliesCoffee = ctx.MkEq(ctx.MkIntConst("Green"),ctx.MkIntConst("Coffee"))
    //the person who smokes Pall Mall rears birds
    let pallmallImpliesBirds = ctx.MkEq(ctx.MkIntConst("PallMall"),ctx.MkIntConst("Birds"))
    //the owner of the yellow house smokes Dunhill
    let yellowImpliesDunhill = ctx.MkEq(ctx.MkIntConst("Yellow"),ctx.MkIntConst("Dunhill"))
    //the man living in the center house drinks milk
    let middleImpliesMilk = ctx.MkEq(ctx.MkIntConst("Milk"),ctx.MkInt(3))
    //the Norwegian lives in the first house
    let norwegianImpliesOne = ctx.MkEq(ctx.MkIntConst("Norwegian"),ctx.MkInt(1))
    //the man who smokes blends lives next to the one who keeps cats
    let blendImpliesCatsNeighbour = ctx.MkOr(ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkAdd(ctx.MkIntConst("Cats"),ctx.MkInt(1))),ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkSub(ctx.MkIntConst("Cats"),ctx.MkInt(1))))
    //the man who keeps horses lives next to the man who smokes Dunhill
    let horsesImpliesDunhillNeighbour = ctx.MkOr(ctx.MkEq(ctx.MkIntConst("Horses"),ctx.MkAdd(ctx.MkIntConst("Dunhill"),ctx.MkInt(1))),ctx.MkEq(ctx.MkIntConst("Horses"),ctx.MkSub(ctx.MkIntConst("Dunhill"),ctx.MkInt(1))))
    //the owner who smokes BlueMaster drinks beer
    let bluemasterImpliesBeer = ctx.MkEq(ctx.MkIntConst("BlueMaster"),ctx.MkIntConst("Beer"))
    //the German smokes Prince
    let germanImpliesPrince = ctx.MkEq(ctx.MkIntConst("German"),ctx.MkIntConst("Prince"))
    //the Norwegian lives next to the blue house
    let norwegianImpliesBlueNeighbour = ctx.MkOr(ctx.MkEq(ctx.MkIntConst("Norwegian"),ctx.MkAdd(ctx.MkIntConst("Blue"),ctx.MkInt(1))),ctx.MkEq(ctx.MkIntConst("Norwegian"),ctx.MkSub(ctx.MkIntConst("Blue"),ctx.MkInt(1))))
    //the man who smokes blend has a neighbor who drinks water
    let blendImpliesWaterNeighbour = ctx.MkOr(ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkAdd(ctx.MkIntConst("Water"),ctx.MkInt(1))),ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkSub(ctx.MkIntConst("Water"),ctx.MkInt(1))))
    let s = ctx.MkSolver()
    let constraints = ctx.MkAnd( [| streetRange
                                    //If you don't explictly constrain the values to be different you don't get a model
                                    distinctNat
                                    distinctCol
                                    distinctDri
                                    distinctSmo
                                    distinctPet
                                    britImpliesRed ;
                                    swedeImpliesDogs; 
                                    daneImpliesTea; 
                                    greenImpliesCoffee; 
                                    greenImpliesWhiteMinusOne; 
                                    pallmallImpliesBirds; 
                                    yellowImpliesDunhill;
                                    middleImpliesMilk;
                                    norwegianImpliesOne
                                    blendImpliesCatsNeighbour
                                    horsesImpliesDunhillNeighbour
                                    bluemasterImpliesBeer
                                    germanImpliesPrince
                                    norwegianImpliesBlueNeighbour
                                    blendImpliesWaterNeighbour 
                                    |] )
    s.Add(constraints)
    match s.Check([||]) with
    | Status.SATISFIABLE -> 
        printf "Sat\n"
        let number char =   Array.map (fun (item:IntExpr) -> (int(sprintf "%O" (s.Model.ConstInterp(item))),(sprintf "%O" item)) ) char
                            |> Map.ofArray
        // let a = number nationalities]
        printf "\t%-10d\t%-10d\t%-10d\t%-10d\t%-10d\n" 1 2 3 4 5
        for i = 1 to 5 do
            printf "%10s\t" (number nationalities).[i]
        printf "\n"
        for i = 1 to 5 do
            printf "%10s\t" (number houseColours).[i]
        printf "\n"
        for i = 1 to 5 do
            printf "%10s\t" (number smokes).[i]
        printf "\n"
        for i = 1 to 5 do
            printf "%10s\t" (number drinks).[i]
        printf "\n"
        for i = 1 to 5 do
            printf "%10s\t" (number pets).[i]
        printf "\n"
        ()
    | Status.UNSATISFIABLE -> printf "Unsat\n"
    | _ -> failwith "Unknown response from solver"
    
main ()



