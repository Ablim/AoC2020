module MemoryGame

let rec private play (map: Map<bigint, bigint list>) lastSpoken index stopIndex =
    if index > stopIndex then
        lastSpoken
    else
        let previous = map.Item lastSpoken
        let nextSpoken =
            if previous.Length = 1 then
                0I
            else
                previous.[0] - previous.[1]
        let nextList =
            match map.TryFind nextSpoken with
            | None -> [ index ]
            | Some x -> (index::x).[..1]
        let nextMap = map.Add(nextSpoken, nextList)
        play nextMap nextSpoken (index+1I) stopIndex

let playMemoryGame (input: string []) numberSpoken =
    let rec seedMap (map: Map<bigint, bigint list>) seed index =
        match seed with
        | [] -> map
        | h::t ->
            match map.TryFind(h) with
            | None ->
                let newMap = map.Add(h, [ index ])
                seedMap newMap t (index+1I)
            | Some x ->
                let newList = (index::x).[..1]
                let newMap = map.Add(h, newList)
                seedMap newMap t (index+1I)
    let seed =
        input.[0].Split(",") |>
            Seq.map (fun x -> bigint.Parse x) |>
                Seq.rev |>
                    Seq.toList
    let map = seedMap Map.empty (seed |> Seq.rev |> Seq.toList) 1I
    play map seed.Head (bigint(seed.Length)+1I) numberSpoken