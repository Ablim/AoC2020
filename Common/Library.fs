namespace Common

module Common =
    let splitIntoGroups (rows : string list) =
        let rec loop rows current all =
            match rows with
            | [] -> current :: all
            | head :: tail ->
                match head with
                | "" -> loop tail [] (current :: all)
                | x -> loop tail (x :: current) all
        loop rows [] []
    
    let merge (rows : string list) =
        let rec loop rows result =
            match rows with
            | [] -> result
            | head :: tail -> loop tail (head + result)
        loop rows ""
    
    let listLength (list : 'a list) =
        list.Length

    let parseInt (value : string) =
        try
            Some (value |> int)
        with
            _ -> None
