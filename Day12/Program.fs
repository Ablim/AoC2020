open System.IO
open Navigation

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt" |> Seq.toList
    
    printfn "Day 12"
    // Part 1
    let output = navigate input |> getManhattanDist
    printfn "Answer to part 1 is %i" output

    // Part 2
    let output = navigateWaypoint input |> getManhattanDist
    printfn "Answer to part 2 is %i" output

    0 // return an integer exit code
