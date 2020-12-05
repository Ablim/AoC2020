open System.IO
open BoardingPass

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    // Part 1
    let topId = 
        Seq.map getId input |>
            Seq.max
    printfn "Answer to part 1 is %i" topId

    // Part 2
    let ids =
        Seq.map getId input |>
            Seq.sort |>
                Seq.toList
    let missingId = findMissingId ids
    printfn "Answer to part 2 is %i" missingId
    0 // return an integer exit code
