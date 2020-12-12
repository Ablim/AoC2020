open System.IO
open Navigation

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt" |> Seq.toList
    
    // Part 1
    let output = getManhattanDist input
    printfn "Answer to part 1 is %i" output

    0 // return an integer exit code
