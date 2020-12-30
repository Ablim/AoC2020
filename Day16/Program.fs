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

    let output = getDepartureMultiple input
    printfn "Answer to part 2 is %A" output
    0 // return an integer exit code
