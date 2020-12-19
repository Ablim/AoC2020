open System.IO
open SeatingSystem

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    printfn "Day 11"
    let output = generateSeating1 input |> countOccupied
    printfn "Answer to part 1 is %i" output
    let output2 = generateSeating2 input |> countOccupied
    printfn "Answer to part 2 is %i" output2

    0 // return an integer exit code
