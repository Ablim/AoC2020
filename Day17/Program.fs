open System.IO
open ConwayCubes

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    printfn "Day 17"
    //Part 1
    let output = sixCycleBoot input
    printfn "Answer to part 1 is %i" output

    0 // return an integer exit code
