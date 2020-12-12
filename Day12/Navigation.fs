module Navigation

type Coordinate =
    {
        Latitude: int
        Longitude: int
    }

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

let getManhattanDist instructions =
    let coords = navigate instructions
    (abs coords.Latitude) + (abs coords.Longitude)