module Adapters

let private canConnect c1 c2 =
    abs (c1 - c2) <= 3

let rec private areConnected adapters =
    match adapters with
    | one::two::tail ->
        if canConnect one two  then
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
    printfn "Chain is connected: %b" (areConnected sorted)
    getJoltDiffsBy sorted (fun x -> 1 + x) * getJoltDiffsBy sorted (fun x -> 3 + x)

let private branchAndPrune (connections: int list) wall device =
    let rec loop (remaining: int list) lastConnection =
        match remaining with
        | [] ->
            //if acc % 10000000 = 0 then printfn "%i" acc
            if canConnect lastConnection device then 1 else 0
        | h::t ->
            // Too much distance
            if not (canConnect lastConnection h) then
                0
            // h must remain
            elif t.Length > 0 && not (canConnect lastConnection t.Head) then
                loop t h
            // h is optional, try with and without
            else
                loop t h + loop t lastConnection
    loop connections wall

let countCombinations adapters =
    let deviceJolt = 3 + Seq.max adapters
    let sorted = Seq.sort adapters |> Seq.toList
    branchAndPrune sorted 0 deviceJolt

let sortAndPrint adapters =
    let max = Seq.max adapters
    let sorted = Seq.sort (0::max::adapters)
    Seq.iter (fun x -> printfn "%i" x) sorted