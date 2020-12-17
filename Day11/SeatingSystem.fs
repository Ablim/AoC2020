module SeatingSystem

let private createMap (input: string []) =
    Seq.map (fun x -> x |> Seq.toArray) input |>
        Seq.toArray

let private copyMap map =
    Seq.map (fun x -> Array.copy x) map |>
        Seq.toArray

let private applyRules (map: char [][]) =
    let mutable currentMap = map
    let mutable nextMap = copyMap map
    let totRows = map.Length
    let totCols = map.[0].Length

    let getAdjacentOccupiedCount (map: char [][]) row col =
        seq {
            for i in (row-1)..(row+1) do
                for j in (col-1)..(col+1) do
                    if i >= 0 && i < totRows && j >= 0 && j < totCols && not (i = row && j = col) then 
                        map.[i].[j]
        } |>
            Seq.where (fun x -> x = '#') |>
                Seq.length

    let canFlip row col =
        if currentMap.[row].[col] = 'L' then
            getAdjacentOccupiedCount currentMap row col = 0
        else
            getAdjacentOccupiedCount currentMap row col >= 4

    let rec colLoop row col changes =
        if col = totCols then
            changes
        else
            let cell = currentMap.[row].[col]
            match cell with
            | 'L' -> 
                if canFlip row col then
                    nextMap.[row].[col] <- '#'
                    colLoop row (col+1) (changes+1)
                else
                    colLoop row (col+1) changes
            | '#' -> 
                if canFlip row col then
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

let generateSeating (input: string []) =
    let seatMap = createMap input
    applyRules seatMap

let countOccupied (map: char [][]) =
    Seq.collect (fun x -> Seq.where (fun y -> y = '#') x) map |>
        Seq.length
