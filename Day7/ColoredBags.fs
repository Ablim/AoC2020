module ColoredBags

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

let bagSearchDown (rows : string list) =
    0