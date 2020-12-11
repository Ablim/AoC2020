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
    bigint (getJoltDiffsBy sorted (fun x -> 1 + x) * getJoltDiffsBy sorted (fun x -> 3 + x))

let private branchAndPrune (connections: int list) wall device =
    let mutable lookup = Map.empty
    let rec loop (remaining: int list) lastConnection =
        match remaining with
        | [] ->
            if canConnect lastConnection device then bigint 1 else bigint 0
        | h::t ->
            // Too much distance
            if not (canConnect lastConnection h) then
                bigint 0
            // h must remain
            elif t.Length > 0 && not (canConnect lastConnection t.Head) then
                match lookup.TryFind h with
                | None ->
                    let v = loop t h
                    lookup <- lookup.Add(h, v)
                    v
                | Some v -> v
            // h is optional, try with and without
            else
                let left =
                    match lookup.TryFind h with
                    | None ->
                        let l = loop t h
                        lookup <- lookup.Add(h, l)
                        l
                    | Some l -> l
                let right = loop t lastConnection
                lookup <- lookup.Add(lastConnection, left + right)
                left + right
    loop connections wall

let countCombinations adapters =
    let deviceJolt = 3 + Seq.max adapters
    let sorted = Seq.sort adapters |> Seq.toList
    branchAndPrune sorted 0 deviceJolt

let sortAndPrint adapters =
    let max = Seq.max adapters
    let sorted = Seq.sort (0::max::adapters)
    Seq.iter (fun x -> printfn "%i" x) sorted