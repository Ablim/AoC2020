module BoardingPass

(*
Row 0 to 127
F means lower half
B means upper half

Column 0 to 7
L means lower half
R means upper half
*)

let even x =
    x % 2 = 0

let lower (list : 'a list) =
    let length = list.Length
    if length <= 1 then
        list
    elif even length then
        let i = length / 2 - 1
        list.[0..i]
    else
        let i = length / 2
        list.[0..i]

let upper (list : 'a list) =
    let length = list.Length
    if length <= 1 then
        list
    elif even length then
        let i = length / 2
        list.[i..]
    else
        let i = length / 2 + 1
        list.[i..]

let getRow (pass : string) =
    let path = Seq.toList pass.[..6]
    let rec search path (rows : int list) =
        match path with
        | [] -> rows.[0]
        | head :: tail ->
            match head with
            | 'F' -> search tail (lower rows) 
            | 'B' -> search tail (upper rows)
            | _ -> 0
    search path [0..127]

let getColumn (pass : string) =
    let path = Seq.toList pass.[7..]
    let rec search path (cols : int list) =
        match path with
        | [] -> cols.[0]
        | head :: tail ->
            match head with
            | 'L' -> search tail (lower cols) 
            | 'R' -> search tail (upper cols)
            | _ -> 0
    search path [0..7]

let getId (pass : string) =
    let row = getRow pass
    let col = getColumn pass
    row * 8 + col

let rec findMissingId (ids : int list) =
    match ids with
    | [] -> 0
    | head :: [] -> head + 1
    | head :: tail ->
        if head + 1 = tail.[0] then
            findMissingId tail
        else
            head + 1