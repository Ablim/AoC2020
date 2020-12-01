open System.IO

let rec find3Helper2 x y listC =
    match listC with
    | [] -> 0
    | head :: tail -> 
        match x + y + head with
        | 2020 -> x * y * head
        | _ -> find3Helper2 x y tail

let rec find3Helper1 x listB listC =
    match listB with
    | [] -> 0
    | head :: tail ->
        match find3Helper2 x head listC with
        | 0 -> find3Helper1 x tail listC
        | x -> x

let rec find3 listA listB listC =
    match listA with
    | [] -> 0
    | head :: tail -> 
        match find3Helper1 head listB listC with
        | 0 -> find3 tail listB listC
        | x -> x

let rec find2 subList list =
    let rec findHelper x list =
        match list with 
        | [] -> 0
        | head :: tail -> 
            match x + head with
            | 2020 -> x * head
            | _ -> findHelper x tail

    match subList with
    | [] -> 0
    | head :: tail -> 
        match findHelper head list with
        | 0 -> find2 tail list
        | x -> x

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("Input.txt")
    let data = Seq.toList (Seq.map (fun x -> x |> int) input)
    
    let answer1 = find2 data data
    printfn "%i" answer1
    
    let answer2 = find3 data data data
    printfn "%i" answer2
    0
