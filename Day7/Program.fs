open System.IO
open ColoredBags

[<EntryPoint>]
let main argv =
    let data = Seq.toList (File.ReadAllLines "Input.txt")

    // Part 1
    printfn "Answer to part 1 is %i" (bagSearchUp data).Length
    
    // Part 2
    printfn "Answer to part 2 is %i" (bagSearchDown data)

    0 // return an integer exit code
