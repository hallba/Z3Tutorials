#I @"C:\Users\bh418\Source\repos\compose-z3-tutorial\platform\Z3x64.4.4.1\lib";;
#r "Microsoft.Z3.dll"

open Microsoft.Z3 


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
    //Now look at the facts (NB houses read 1 to 5 left to right)
    let britImpliesRed = ctx.MkEq(ctx.MkIntConst("Briton"),ctx.MkIntConst("Red"))
    let swedeImpliesDogs = ctx.MkEq(ctx.MkIntConst("Swede"),ctx.MkIntConst("Dogs"))
    let daneImpliesTea = ctx.MkEq(ctx.MkIntConst("Dane"),ctx.MkIntConst("Tea"))
    let greenImpliesWhiteMinusOne = ctx.MkEq(ctx.MkIntConst("White"),ctx.MkSub(ctx.MkIntConst("Green"),ctx.MkInt(1)))
    let greenImpliesCoffee = ctx.MkEq(ctx.MkIntConst("Green"),ctx.MkIntConst("Coffee"))
    let pallmallImpliesBirds = ctx.MkEq(ctx.MkIntConst("PallMall"),ctx.MkIntConst("Birds"))
    let yellowImpliesDunhill = ctx.MkEq(ctx.MkIntConst("Yellow"),ctx.MkIntConst("Dunhill"))
    let middleImpliesMilk = ctx.MkEq(ctx.MkIntConst("Milk"),ctx.MkInt(3))
    let norwegianImpliesOne = ctx.MkEq(ctx.MkIntConst("Norwegian"),ctx.MkInt(1))
    let blendImpliesCats = ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkIntConst("Cats"))
    let horsesImpliesDunhill = ctx.MkEq(ctx.MkIntConst("Horses"),ctx.MkIntConst("Dunhill"))
    let bluemasterImpliesBeer = ctx.MkEq(ctx.MkIntConst("BlueMaster"),ctx.MkIntConst("Beer"))
    let germanImpliesPrince = ctx.MkEq(ctx.MkIntConst("German"),ctx.MkIntConst("Prince"))
    let norwegianImpliesBlueNeighbour = ctx.MkOr(ctx.MkEq(ctx.MkIntConst("Norwegian"),ctx.MkAdd(ctx.MkIntConst("Blue"),ctx.MkInt(1))),ctx.MkEq(ctx.MkIntConst("Norwegian"),ctx.MkSub(ctx.MkIntConst("Blue"),ctx.MkInt(1))))
    let blendImpliesWaterNeighbour = ctx.MkOr(ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkAdd(ctx.MkIntConst("Water"),ctx.MkInt(1))),ctx.MkEq(ctx.MkIntConst("Blend"),ctx.MkSub(ctx.MkIntConst("Water"),ctx.MkInt(1))))
    let s = ctx.MkSolver()
    let constraints = ctx.MkAnd( [| streetRange
                                    britImpliesRed ;
                                    swedeImpliesDogs; 
                                    daneImpliesTea; 
                                    greenImpliesCoffee; 
                                    greenImpliesWhiteMinusOne; 
                                    pallmallImpliesBirds; 
                                    yellowImpliesDunhill;
                                    middleImpliesMilk;
                                    norwegianImpliesOne
                                    blendImpliesCats
                                    horsesImpliesDunhill
                                    bluemasterImpliesBeer
                                    germanImpliesPrince
                                    norwegianImpliesBlueNeighbour
                                    blendImpliesWaterNeighbour |] )
    match s.Check([||]) with
    | Status.SATISFIABLE -> 
        printf "Sat\n"
        let number char =   Array.map (fun (item:IntExpr) -> sprintf "%O" (s.Model.ConstInterp(item)) ) char
                            |> String.concat "\t"
        let a = number nationalities
        // printf "%s\n" (number nationalities)
        // printf "%s\n" (number houseColours)
        // printf "%s\n" (number drinks)
        // printf "%s\n" (number smokes)
        // printf "%s\n" (number pets)
        ()
    | Status.UNSATISFIABLE -> printf "Unsat\n"
    | _ -> failwith "Unknown response from solver"
    
main ()



