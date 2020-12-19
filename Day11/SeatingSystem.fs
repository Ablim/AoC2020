module SeatingSystem

let private createMap (input: string []) =
    Seq.map (fun x -> x |> Seq.toArray) input |>
        Seq.toArray

let private copyMap map =
    Seq.map (fun x -> Array.copy x) map |>
        Seq.toArray

let private getAdjacentOccupiedCount (map: char [][]) row col totRows totCols =
    seq {
        for i in (row-1)..(row+1) do
            for j in (col-1)..(col+1) do
                if i >= 0 && i < totRows && j >= 0 && j < totCols && not (i = row && j = col) then 
                    map.[i].[j]
    } |>
        Seq.where (fun x -> x = '#') |>
            Seq.length

let private canFlip (map: char [][]) row col totRows totCols =
    if map.[row].[col] = 'L' then
        getAdjacentOccupiedCount map row col totRows totCols = 0
    else
        getAdjacentOccupiedCount map row col totRows totCols >= 4

let private getOccupiedInSightCount (map: char [][]) row col totRows totCols =
    let rec laser r c dR dC =
        if r >= 0 && r < totRows && c >= 0 && c < totCols then
            match map.[r].[c] with
            | '#' -> 1
            | 'L' -> 0
            | '.' -> laser (r+dR) (c+dC) dR dC
            | x -> failwithf "%c is invalid" x
        else
            0
    
    let directions = [ (-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1) ]
    Seq.map (fun x ->
        let first = fst x
        let second = snd x
        laser (row+first) (col+second) first second) directions |>
            Seq.sum

let private canFlip2 (map: char [][]) row col totRows totCols =
    if map.[row].[col] = 'L' then
        getOccupiedInSightCount map row col totRows totCols = 0
    else
        getOccupiedInSightCount map row col totRows totCols >= 5

let private applyRules (map: char [][]) ruleNo =
    let mutable currentMap = map
    let mutable nextMap = copyMap map
    let totRows = map.Length
    let totCols = map.[0].Length
    let rule = if ruleNo = 1 then canFlip else canFlip2

    let rec colLoop row col changes =
        if col = totCols then
            changes
        else
            let cell = currentMap.[row].[col]
            match cell with
            | 'L' -> 
                if rule currentMap row col totRows totCols then
                    nextMap.[row].[col] <- '#'
                    colLoop row (col+1) (changes+1)
                else
                    colLoop row (col+1) changes
            | '#' -> 
                if rule currentMap row col totRows totCols then
                    nextMap.[row].[col] <- 'L'
                    colLoop row (col+1) (changes+1)
                else
                    colLoop row (col+1) changes
            | '.' -> colLoop row (col+1) changes
            | _ -> failwithf "%c" cell

    let rec rowLoop row changes =
        if row = totRows then
            changes
        else
            rowLoop (row+1) (changes+colLoop row 0 0)

    let rec mainLoop () =
        let changes = rowLoop 0 0
        if changes = 0 then
            nextMap
        else
            currentMap <- nextMap
            nextMap <- copyMap nextMap
            mainLoop ()

    mainLoop ()

let generateSeating1 (input: string []) =
    let seatMap = createMap input
    applyRules seatMap 1

let generateSeating2 (input: string []) =
    let seatMap = createMap input
    applyRules seatMap 2
    
let countOccupied (map: char [][]) =
    Seq.collect (fun x -> Seq.where (fun y -> y = '#') x) map |>
        Seq.length
