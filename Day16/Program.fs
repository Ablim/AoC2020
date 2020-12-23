open System.IO
open TicketTranslation

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "Input.txt" |>
            Seq.toList

    printfn "Day 16"
    let output = getScanningErrorRate input
    printfn "Answer to part 1 is %i" output

    0 // return an integer exit code
