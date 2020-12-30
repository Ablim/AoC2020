open System.Diagnostics
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

    let timer = Stopwatch.StartNew()
    let output = getDepartureMultiple input
    timer.Stop()
    printfn "Answer to part 2 is %A in %i ms" output timer.ElapsedMilliseconds
    0 // return an integer exit code
