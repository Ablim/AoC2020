module Adapters

let rec private areConnected adapters =
    match adapters with
    | one::two::tail ->
        if abs (one - two) <= 3 then
            areConnected (two::tail)
        else
            false
    | _ -> true

let private getJoltDiffsBy adapters (f: int -> int) =
    let rec loop adapters acc =
        match adapters with
        | a::b::tail ->
            if f a = b then
                loop (b::tail) (acc+1)
            else
                loop (b::tail) acc
        | _ -> acc
    loop adapters 0

let connect adapters =
    let deviceJolt = 3 + Seq.max adapters
    let sorted = Seq.sort (0::deviceJolt::adapters) |> Seq.toList
    getJoltDiffsBy sorted (fun x -> 1 + x) * getJoltDiffsBy sorted (fun x -> 3 + x)

let private checkEnds (connections: int list) left right =
    connections.Length > 0
    && (connections.[0] = left
        && connections.[connections.Length-1] = right
        || connections.[0] = right
        && connections.[connections.Length-1] = left)

let private branchAndPrune (connections: int list) wall device =
    let rec loop (remaining: int list) selected =
        if not (areConnected selected) then
            0
        else
            match remaining with
            | [] ->
                if checkEnds selected wall device then
                    1
                else
                    0
            | h::t -> (loop t (h::selected)) + (loop t selected)
    loop connections []

let countCombinations adapters =
    let deviceJolt = 3 + Seq.max adapters
    let allConnections = 0::deviceJolt::adapters
    let sorted = Seq.sort allConnections |> Seq.toList
    branchAndPrune sorted 0 deviceJolt