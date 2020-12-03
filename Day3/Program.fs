open System.IO

let isTree x = x = '#'

let toboggan2 map down right =
    let rec search (map : string list) downSteps col rightSteps trees = 
        match map with
        | [] -> trees
        | head :: _ ->
            let index = col % head.Length
            let place = head.[index]
            let tail = map.[downSteps..]
            let newTrees = if isTree place then trees + 1 else trees
            search tail downSteps (col + rightSteps) rightSteps newTrees
    search map down 0 right 0

let toboggan map = 
    let rec search (map : string list) col trees = 
        match map with
        | [] -> trees
        | head :: tail ->
            let index = col % head.Length
            let place = head.[index]
            if isTree place then search tail (col + 3) (trees + 1)
            else search tail (col + 3) trees
    search map 0 0

[<EntryPoint>]
let main argv =
    let input = Seq.toList (File.ReadAllLines "Input.txt")
    
    // Part 1
    let answer = toboggan input
    printfn "The answer to part 1 is %i" answer

    // Part 2
    let slope1 = toboggan2 input 1 1 |> bigint
    let slope2 = toboggan2 input 1 3 |> bigint
    let slope3 = toboggan2 input 1 5 |> bigint
    let slope4 = toboggan2 input 1 7 |> bigint
    let slope5 = toboggan2 input 2 1 |> bigint

    printfn "Slope 1 has %A" slope1
    printfn "Slope 2 has %A" slope2
    printfn "Slope 3 has %A" slope3
    printfn "Slope 4 has %A" slope4
    printfn "Slope 5 has %A" slope5

    printfn "The answer to part 2 is %A" (slope1 * slope2 * slope3 * slope4 * slope5)
    0 // return an integer exit code
