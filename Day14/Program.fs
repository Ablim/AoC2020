open System.IO
open DockingSystem

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "Input.txt" |>
            Seq.toList

    printfn "Day 14"
    let output = initializeMemory input |> sumMemory
    printfn "Answer to part 1 is %A" output
    0 // return an integer exit code
