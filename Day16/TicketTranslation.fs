module TicketTranslation

let private parseIntervals input =
    let rec loop rows (intervals: seq<int * int>) =
        match rows with
        | [] -> failwith "Should not happen"
        | h::t ->
            if h = "" then
                intervals |> Seq.toList
            else
                let newIntervals =
                    h.Split(" ") |>
                        Seq.where (fun x -> x.Contains("-")) |>
                            Seq.map (fun x ->
                                let parts = x.Split("-")
                                (parts.[0] |> int, parts.[1] |> int))
                loop t (Seq.append intervals newIntervals)
    loop input []

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

let getScanningErrorRate input =
    let intervals = parseIntervals input
    let maxValue =
        Seq.map (fun x -> max (fst x) (snd x)) intervals |>
            Seq.max
    let nearbyTickets = parseNearbyTickets input
    
    let intervalControl = Array.zeroCreate<int> (maxValue+1)
    Seq.iter (fun x ->
        let left = fst x
        let right = snd x
        let indices = [left..right]
        Seq.iter (fun y -> intervalControl.[y] <- 1) indices
        ) intervals

    Seq.concat nearbyTickets |>
        Seq.where (fun x ->
            x < 0
            || x > maxValue
            || intervalControl.[x] = 0) |>
                Seq.sum