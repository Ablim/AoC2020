module ColoredBags

open Common.Common

let rec listContains (list : string list) item =
    match list with
    | [] -> false
    | head :: tail -> 
        if head = item then
            true
        else
            listContains tail item

let trimBag (bag : string) =
    bag.Replace (" bags", "")

let bagSearchUp (rows : string list) =
    let divided =
        Seq.map (fun (x : string) -> x.Split " contain ") rows |>
            Seq.toList
    let rec loop rules (searchBags : string list) (foundBags : string list) =
        match rules with
        | [] ->
            if searchBags.Length <= 1 then
                foundBags
            else
                let i = searchBags.Length - 2
                loop divided searchBags.[..i] foundBags // Remove last
        | head :: tail ->
            let bagToSearchFor = searchBags.[searchBags.Length - 1]
            let candidate = trimBag head.[0]
            let searchSpace = head.[1]

            if searchSpace.Contains bagToSearchFor && not (listContains foundBags candidate) then
                loop tail (candidate :: searchBags) (candidate :: foundBags)
            else
                loop tail searchBags foundBags
    loop divided ["shiny gold"] []

let parseBagRow (row : string) =
    let parts = row.Split " "
    match parseInt parts.[0] with
    | Some _ -> [| parts.[0]; $"{parts.[1]} {parts.[2]}" |]
    | None -> [||]

let bagSearchDown (rows : string list) =
    let divided =
        Seq.map (fun (x : string) -> x.Split " contain ") rows |>
            Seq.toList

    let rec loop (rules : string [] list) (target : string) =
        match rules with
        | [] -> 0
        | h :: t ->
            if h.[0].Contains target then
                h.[1].Split ", " |>
                    Seq.map parseBagRow |>
                        Seq.map (fun x ->
                            if x.Length = 0 then
                                0
                            else
                                let factor = x.[0] |> int
                                factor + factor * (loop divided x.[1])) |>
                                Seq.sum
            else
                loop t target
    loop divided "shiny gold"