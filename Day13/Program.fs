open System.IO
open BusPlanner

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "Input.txt"

    // Part 1
    let output = getBusWaitFactor (input.[0] |> int) input.[1]
    printfn "Answer to part 1 is %i" output

    // Part 2
    let output = getSchedule input.[1]
    printfn "Answer to part 2 is %A" output

    0 // return an integer exit code
