open System.IO

let count (c : char) (word : string) =
    let rec loop c word counter =
        match word with
        | [] -> counter
        | head :: tail -> 
            if c = head then loop c tail counter + 1
            else loop c tail counter
    loop c (Seq.toList word) 0

let isValid (password : string) rule =
    let parts = Seq.toList (password.Split ' ')

    match parts with
    | head :: body :: tail :: [] -> 
        let conditions = head.Split '-'
        let indexLeft = conditions.[0] |> int
        let indexRight = conditions.[1] |> int
        let letter = body.[0]
        
        if rule = 1 then
            let count = count letter tail
            if indexLeft <= count && count <= indexRight then 1 else 0
        else
            let firstCheck = letter = tail.[indexLeft - 1]
            let secondCheck = letter = tail.[indexRight - 1]
            if firstCheck <> secondCheck then 1 else 0
    | _ -> 0

let getValidCount input rule =
    Seq.sum (Seq.map (fun x -> (isValid x rule)) input)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("Input.txt")

    // Part 1
    let data = getValidCount input 1
    printfn "Answer to part 1 is %i" data

    // Part 2
    let data = getValidCount input 2
    printfn "Answer to part 2 is %i" data

    // XOR :D
    printfn "%b" (false <> false)
    printfn "%b" (false <> true)
    printfn "%b" (true <> false)
    printfn "%b" (true <> true)
    0
