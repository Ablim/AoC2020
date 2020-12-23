open System.IO
open MemoryGame

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    printfn "Day 15"
    let output = playMemoryGame input 2020I
    printfn "Answer to part 1 is %A" output

    let output = playMemoryGame input 30000000I
    printfn "Answer to part 2 is %A" output

    0 // return an integer exit code
