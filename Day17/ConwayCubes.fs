module ConwayCubes

open System.Collections.Generic

let private createCube rows =
    let rec addRow pairs (cube: Map<(int * int * int), char>) yIndex zIndex =
        match Seq.tryHead pairs with
        | None -> cube
        | Some (xIndex, state) ->
            let coordinate = (xIndex, yIndex, zIndex)
            let newCube = cube.Add(coordinate, state)
            addRow (Seq.tail pairs) newCube yIndex zIndex
    
    let rec loop rows cube yIndex =
        match Seq.tryHead rows with
        | None -> cube
        | Some row ->
            let splittedRow = row |> Seq.toArray
            let indices = [0..(splittedRow.Length-1)]
            let zipped = Seq.zip indices splittedRow
            let newCube = addRow zipped cube yIndex 0
            loop (Seq.tail rows) newCube (yIndex+1)
    
    loop rows Map.empty 0

let private getMinMaxIndices cube =
    let rec loop (cube: seq<KeyValuePair<(int * int * int), char>>) minX maxX minY maxY minZ maxZ =
        match Seq.tryHead cube with
        | None -> (minX, maxX, minY, maxY, minZ, maxZ)
        | Some c ->
            let (x, y, z) = c.Key
            let newMinX = min x minX
            let newMinY = min y minY
            let newMinZ = min z minZ
            let newMaxX = max x maxX
            let newMaxY = max y maxY
            let newMaxZ = max z maxZ
            loop (Seq.tail cube) newMinX newMaxX newMinY newMaxY newMinZ newMaxZ

    loop cube 0 0 0 0 0 0

let rec addInactives indices (cube: Map<(int * int * int), char>) =
    match Seq.tryHead indices with
    | None -> cube
    | Some index -> 
        let newCube = cube.Add(index, '.')
        addInactives (Seq.tail indices) newCube

let rec private addInactiveNeighbors cube =
    let (minX, maxX, minY, maxY, minZ, maxZ) = getMinMaxIndices cube
    let a = 
        seq {
            for x in minX-1..maxX+1 do
                for y in minY-1..maxY+1 do
                    (x, y, minZ-1)
        }
    let b = 
        seq {
            for x in minX-1..maxX+1 do
                for y in minY-1..maxY+1 do
                    (x, y, maxZ+1)
        }
    let c = 
        seq {
            for x in minX-1..maxX+1 do
                for z in minZ-1..maxZ+1 do
                    (x, minY-1, z)
        }
    let d = 
        seq {
            for x in minX-1..maxX+1 do
                for z in minZ-1..maxZ+1 do
                    (x, maxY+1, z)
        }
    let e = 
        seq {
            for y in minY-1..maxY+1 do
                for z in minZ-1..maxZ+1 do
                    (minX-1, y, z)
        }
    let f = 
        seq {
            for y in minY-1..maxY+1 do
                for z in minZ-1..maxZ+1 do
                    (maxX+1, y, z)
        }
    let newIndices =
        Seq.append a b |>
            Seq.append c |>
                Seq.append d |>
                    Seq.append e |>
                        Seq.append f
    addInactives newIndices cube

let private getNeighbors (x, y, z) =
    seq {
        for tX in x-1..x+1 do
            for tY in y-1..y+1 do
                for tZ in z-1..z+1 do
                    if not (tX = x && tY = y && tZ = z) then
                        (tX, tY, tZ)
    }

let private countActiveNeighbors (cube: Map<(int * int * int), char>) xyzIndex =
    let indices = getNeighbors xyzIndex
    indices |>
        Seq.map (fun x -> cube.TryFind x) |>
            Seq.where (fun x -> x = Some '#') |>
                Seq.length

let rec private cycleOnce wholeCube (partialCube: seq<KeyValuePair<(int * int * int), char>>) (newCube: Map<(int * int * int), char>) =
    match Seq.tryHead partialCube with
    | None -> newCube
    | Some c ->
        let activeNeighbors = countActiveNeighbors wholeCube c.Key
        let active = c.Value = '#'
        let newState = 
            if active then
                if 2 <= activeNeighbors && activeNeighbors <= 3 then '#' else '.'
            else
                if activeNeighbors = 3 then '#' else '.'
        cycleOnce wholeCube (Seq.tail partialCube) (newCube.Add(c.Key, newState))

let rec private cycle cube turns =
    if turns = 0 then
        cube
    else
        printfn "Loop %i" turns
        let filledCube = addInactiveNeighbors cube
        let newCube = cycleOnce filledCube filledCube Map.empty
        cycle newCube (turns-1)

let private countActive (cube: seq<KeyValuePair<(int * int * int), char>>) =
    cube |>
        Seq.filter (fun x -> x.Value = '#') |>
            Seq.length

let sixCycleBoot input =
    let cube = createCube input
    let afterCycle = cycle cube 6
    countActive afterCycle