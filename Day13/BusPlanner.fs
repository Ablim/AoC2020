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
                loop t (offset+1I) departures
            else
                let id = bigint.Parse h
                let dep = { Id = id; Offset = offset }
                loop t (offset+1I) (dep::departures)
    loop busses (0I) [] |>
        Seq.rev |>
            Seq.toList

let getSchedule (busses: string) =
    let rec loop2 funcs timestamp =
        match funcs with
        | [] -> true
        | h::t -> 
            if not (h timestamp = 0I) then
                false
            else
                loop2 t timestamp

    let rec loop funcs maxId timestamp =
        if loop2 funcs timestamp then
            timestamp
        else
            loop funcs maxId (timestamp+maxId)

    let busList = busses.Split "," |> Seq.toList
    let departures = getDepartures busList
    let funcs =
        Seq.map (fun x -> fun t -> (t+x.Offset) % x.Id) departures |>
            Seq.toList
    let maxId = Seq.map (fun x -> x.Id) departures |> Seq.max
    let maxDep = (Seq.where (fun x -> x.Id = maxId) departures |> Seq.toList).[0]
    loop funcs maxDep.Id (maxDep.Id - maxDep.Offset)