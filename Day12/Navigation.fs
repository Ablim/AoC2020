module Navigation

open System

type Coordinate =
    {
        Latitude: int
        Longitude: int
    } with 
    member this.Add lat long =
        { Latitude = this.Latitude + lat; Longitude = this.Longitude + long }

let private fromDirection direction =
    match direction with
    | 'N' -> 0
    | 'E' -> 1
    | 'S' -> 2
    | 'W' -> 3
    | _ -> invalidArg (nameof direction) "Invalid direction"

let private toDirection direction =
    let dir = (direction + 4) % 4
    match dir with
    | 0 -> 'N'
    | 1 -> 'E'
    | 2 -> 'S'
    | 3 -> 'W'
    | _ -> invalidArg (nameof direction)  "Invalid direction"

let private rotateRight (direction: char) degrees =
    if degrees % 360 = 0 then
        direction
    elif degrees % 270 = 0 then
        3 + fromDirection direction |> toDirection
    elif degrees % 180 = 0 then
        2 + fromDirection direction |> toDirection
    elif degrees % 90 = 0 then
        1 + fromDirection direction |> toDirection
    else
        invalidArg (nameof degrees) "Invalid number of degrees"

let private rotateLeft (direction: char) degrees =
    if degrees % 360 = 0 then
        direction
    elif degrees % 270 = 0 then
        (- 3) + fromDirection direction |> toDirection
    elif degrees % 180 = 0 then
        (- 2) + fromDirection direction |> toDirection
    elif degrees % 90 = 0 then
        (- 1) + fromDirection direction |> toDirection
    else
        invalidArg (nameof degrees) "Invalid number of degrees"

let navigate instructions =
    let rec loop (instructions: string list) lat long dir =
        match instructions with
        | [] -> { Latitude = lat; Longitude = long}
        | h::t ->
            let inst = h.[0]
            let mag = h.[1..] |> int
            match inst with
            | 'N' -> loop t (lat+mag) long dir
            | 'S' -> loop t (lat-mag) long dir
            | 'E' -> loop t lat (long+mag) dir
            | 'W' -> loop t lat (long-mag) dir
            | 'R' -> loop t lat long (rotateRight dir mag)
            | 'L' -> loop t lat long (rotateLeft dir mag)
            | 'F' ->
                match dir with
                | 'N' -> loop t (lat+mag) long dir
                | 'S' -> loop t (lat-mag) long dir
                | 'E' -> loop t lat (long+mag) dir
                | 'W' -> loop t lat (long-mag) dir
                | _ -> invalidArg (nameof dir) "Invalid direction"
            | _ -> invalidArg (nameof inst) "Invalid movement"
    loop instructions 0 0 'E'

let getManhattanDist coords =
    (abs coords.Latitude) + (abs coords.Longitude)

let rec private moveBoat (boat: Coordinate) (waypoint: Coordinate) steps =
    match steps with
    | 0 -> boat
    | i -> moveBoat (boat.Add waypoint.Latitude waypoint.Longitude) waypoint (i-1)

let private rotateWaypoint waypoint deg =
    let x = waypoint.Longitude |> float
    let y = waypoint.Latitude |> float
    let degRad = deg * Math.PI / 180.0
    let newX = x * (cos degRad) - y * (sin degRad)
    let newY = x * (sin degRad) + y * (cos degRad)
    let newLat = round newY |> int
    let newLong = round newX |> int
    { Latitude = newLat; Longitude = newLong }

let navigateWaypoint instructions =
    let rec loop (instructions: string list) (boat: Coordinate) (waypoint: Coordinate) =
        match instructions with
        | [] -> { Latitude = boat.Latitude; Longitude = boat.Longitude }
        | h::t ->
            let inst = h.[0]
            let mag = h.[1..] |> int
            match inst with
            | 'N' -> loop t boat (waypoint.Add mag 0)
            | 'S' -> loop t boat (waypoint.Add -mag 0)
            | 'E' -> loop t boat (waypoint.Add 0 mag)
            | 'W' -> loop t boat (waypoint.Add 0 -mag)
            | 'R' -> loop t boat (rotateWaypoint waypoint (-mag |> float))
            | 'L' -> loop t boat (rotateWaypoint waypoint (mag |> float))
            | 'F' -> loop t (moveBoat boat waypoint mag) waypoint
            | _ -> failwithf "Invalid instruction %c" inst
    loop instructions { Latitude = 0; Longitude = 0 } { Latitude = 1; Longitude = 10 }