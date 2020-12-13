module BusPlanner

open System

type Departure =
    {
        Id: bigint
        Offset: bigint
    }

let private getWaitTime departAt bus =
    let rest = departAt % bus
    let next = departAt + bus - rest
    next - departAt

let getBus (timestamp: int) (busses: string) =
    let rec loop activeBusses bestBus bestWait =
        match activeBusses with
        | [] -> bestBus
        | h::t ->
            let wait = getWaitTime timestamp h
            if wait < bestWait then
                loop t h wait
            else
                loop t bestBus bestWait
    
    let activeBusses =
        busses.Split "," |>
            Seq.where (fun x -> not (x = "x")) |>
                Seq.map (fun x -> x |> int) |>
                    Seq.toList
    loop activeBusses 0 Int32.MaxValue

let getBusWaitFactor timestamp busses =
    let bus = getBus timestamp busses
    let wait = getWaitTime timestamp bus
    bus * wait

let private getDepartures busses =
    let rec loop busses offset departures =
        match busses with
        | [] -> departures
        | h :: t ->
            if (h = "x") then
                loop t (offset+bigint 1) departures
            else
                let id = bigint.Parse h
                let dep = { Id = id; Offset = offset }
                loop t (offset+bigint 1) (dep::departures)
    loop busses (bigint 0) [] |>
        Seq.rev |>
            Seq.toList

let getSchedule (busses: string) =
    let busList = busses.Split "," |> Seq.toList
    let departures = getDepartures busList
    bigint 0