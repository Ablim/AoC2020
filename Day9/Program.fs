open System.IO
open XMAS

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "Input.txt" |>
            Seq.map (fun x -> bigint.Parse x) |>
                Seq.toList

    // Part 1
    let value = hackerSearch input 25
    printfn "Answer to part 1 is %A" value

    // Part 2
    let value = findSequence input 25
    printfn "Answer to part 2 is %A" value

    0 // return an integer exit code
