open System.IO
open Boot

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    // Part 1
    let acc = execute input
    printfn "Answer to part 1 is %i" acc.Accumulator

    // Part 2
    let fixedAcc = executeAndFix input
    printfn "Answer to part 2 is %i" fixedAcc
    0 // return an integer exit code
