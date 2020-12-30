module TicketTranslation

let private parseIntervalsWithLabels input =
    let parseLabel (row: string) =
        row.[..(row.IndexOf(":"))]
    let parseRowIntervals (row: string) =
        row.Split(" ") |>
            Seq.where (fun x -> x.Contains("-")) |>
                Seq.map (fun x ->
                    let parts = x.Split("-")
                    (parts.[0] |> int, parts.[1] |> int)) |>
                        Seq.toList
    let rec loop (rows: string list) (intervals: Map<string, (int * int) list>) =
        let row = rows.Head
        match row with
        | "" -> intervals
        | _ ->
            let label = parseLabel row
            let rowIntervals = parseRowIntervals row
            loop rows.Tail (intervals.Add(label, rowIntervals))
    loop input Map.empty

let private parseNearbyTickets input =
    let rec loop (rows: string list) tickets =
        if rows.Head = "nearby tickets:" then
            tickets
        else
            let newRow =
                rows.Head.Split(",") |>
                    Seq.map (fun x -> x |> int) |>
                        Seq.toList
            loop rows.Tail (newRow::tickets)
    loop (Seq.rev input |> Seq.toList) []

let private parseMyTicket input =
    let rec loop (rows: string list) =
        match rows.Head with
        | "your ticket:" ->
            rows.Tail.[0].Split(",") |>
                Seq.map (fun x -> x |> int)
        | _ -> loop rows.Tail
    loop input

let private createIntervalArray intervals =
    let maxValue =
        Seq.map (fun x -> max (fst x) (snd x)) intervals |>
            Seq.max
    let intervalControl = Array.zeroCreate<int> (maxValue+1)
    Seq.iter (fun x ->
        let left = fst x
        let right = snd x
        let indices = [left..right]
        Seq.iter (fun y -> intervalControl.[y] <- 1) indices
        ) intervals
    intervalControl

let getScanningErrorRate input =
    let intervals =
        parseIntervalsWithLabels input |>
            Seq.map (fun x -> x.Value) |>
                Seq.concat
    let nearbyTickets = parseNearbyTickets input
    let intervalControl = createIntervalArray intervals
    Seq.concat nearbyTickets |>
        Seq.where (fun x ->
            x < 0
            || x >= intervalControl.Length
            || intervalControl.[x] = 0) |>
                Seq.sum

let private removeInvalidTickets tickets intervals =
    let intervalControl = createIntervalArray intervals
    let rec isValid ticket =
        match ticket with
        | [] -> true
        | h::t ->
            if h < 0 || h >= intervalControl.Length || intervalControl.[h] = 0 then
                false
            else
                isValid t
    let rec loop allTickets validTickets =
        match allTickets with
        | [] -> validTickets
        | h::t ->
            if isValid h then
                loop t (h::validTickets)
            else
                loop t validTickets
    loop tickets []

let rec private isValueInAnyInterval value intervals =
    match Seq.tryHead intervals with
    | None -> false
    | Some h ->
        let left = fst h
        let right = snd h
        if left <= value && value <= right then
            true
        else
            isValueInAnyInterval value (Seq.tail intervals)

let rec private isSequenceInIntervals values intervals =
    match Seq.tryHead values with
    | None -> true
    | Some h ->
        if not (isValueInAnyInterval h intervals) then
            false
        else
            isSequenceInIntervals (Seq.tail values) intervals

let private createColumns (tickets: seq<'a list>) length =
    seq {
        for i in 0..length do
            seq {
                for t in tickets do
                    t.[i]
            }
    }

let private createMapIndexKey (map: Map<string, 'b>) (index: int) =
    let mapKey =
        map |>
            Seq.map (fun x -> x.Key) |>
                Seq.fold (fun state x -> state + x) ""
    mapKey + index.ToString()

let private findTicketSections labeledIntervals (tickets: int list list) =
    let maxIndex = tickets.Head.Length - 1
    let columns =
        createColumns tickets maxIndex |>
            Seq.toArray
    let mutable lookup = Map.empty
    let mutable branchLookup = Map.empty

    let rec loop (labelMap: Map<string, (int * int) list>) (resultMap: Map<int, string>) index =
        if labelMap.IsEmpty then
            (true, resultMap)
        else
            let branchKey = createMapIndexKey labelMap index
            
            if branchLookup.ContainsKey branchKey then
                branchLookup.[branchKey]
            else
                let thisLevelResult =
                    labelMap |>
                        Seq.map (fun m ->
                            let lookupKey = m.Key + index.ToString()
                            if not (lookup.ContainsKey lookupKey) then
                               lookup <- lookup.Add(lookupKey, isSequenceInIntervals columns.[index] m.Value)
                        
                            if lookup.[lookupKey] then
                                let newMap = labelMap.Remove m.Key
                                let subTreeResult = loop newMap resultMap (index+1)
                                if fst subTreeResult then
                                    (true, (snd subTreeResult).Add(index, m.Key))
                                else
                                    (false, Map.empty)
                            else
                                (false, Map.empty)
                        ) |>
                            Seq.where (fun x -> fst x)
                
                match Seq.tryHead thisLevelResult with
                | None ->
                    branchLookup <- branchLookup.Add(branchKey, (false, Map.empty))
                    (false, Map.empty)
                | Some x ->
                    branchLookup <- branchLookup.Add(branchKey, x)
                    x
    let result = loop labeledIntervals Map.empty 0
    snd result |>
        Seq.sortBy (fun x -> x.Key) |>
            Seq.map (fun x -> x.Value) |>
                Seq.toArray

let private getIndicesOf (word: string) (words: string []) =
    seq {
        for i in 0..(words.Length-1) do
            if words.[i].Contains word then
                i
    }

let getDepartureMultiple input =
    let intervals = parseIntervalsWithLabels input
    let intervalSequence =
        intervals |>
            Seq.map (fun x -> x.Value) |>
                Seq.concat
    let nearbyTickets = parseNearbyTickets input
    let validTickets = removeInvalidTickets nearbyTickets intervalSequence
    let sectionLookup = findTicketSections intervals validTickets
    let myTicket = parseMyTicket input |> Seq.toArray
    getIndicesOf "departure" sectionLookup |>
        Seq.map (fun x -> myTicket.[x]) |>
            Seq.fold (fun state x -> state * (bigint x)) 1I