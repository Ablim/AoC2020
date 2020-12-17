open System.IO
open SeatingSystem

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    printfn "Day 11"
    let output = generateSeating input |> countOccupied
    printfn "Answer to part 1 is %i" output
    printfn "Answer to part 2 is "

    0 // return an integer exit code
