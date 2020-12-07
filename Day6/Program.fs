open System.IO
open CustomCustoms

[<EntryPoint>]
let main argv =
    let input = Seq.toList (File.ReadAllLines "Input.txt")

    // Part 1
    let sum = getQuestionSum input
    printfn "Answer to part 1 is %i" sum

    // Part 2
    let sum2 = getQuestionSum2 input
    printfn "Answer to part 2 is %i" sum2
    0 // return an integer exit code
