open System.IO
open Adapters

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "Input.txt" |>
            Seq.map (fun x -> x |> int) |>
                Seq.toList

    // Part 1
    let output = connect input
    printfn "Answer to part 1 is %A" output

    // Part 2
    let output = countCombinations input
    printfn "Answer to part 2 is %A" output

    0 // return an integer exit code
