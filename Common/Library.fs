namespace Common

module Common =
    type Bezout =
        {
            A: bigint
            X: bigint
            B: bigint
            Y: bigint
            GcdAB: bigint
        }

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

    let removeLast (list:'a list) =
        let reversed = Seq.rev list |> Seq.toList
        match reversed with
        | [] -> []
        | h::t -> Seq.rev t |> Seq.toList

    let addLast (list:'a list) a =
        let reversed = Seq.rev list |> Seq.toList
        a::reversed |>
            Seq.rev |>
                Seq.toList

    let extendedGcd (numA: bigint) (numB: bigint) =
        let rec loop oldR r oldS s oldT t =
            if r = 0I then
                { A = numA; B = numB; GcdAB = oldR; X = oldS; Y = oldT }
            else
                let q = oldR / r
                let tempOldR = r
                let tempR = oldR - q * r
                let tempOldS = s
                let tempS = oldS - q * s
                let tempOldT = t
                let tempT = oldT - q * t
                loop tempOldR tempR tempOldS tempS tempOldT tempT
        
        let a = max numA numB
        let b = min numA numB
        loop a b 1I 0I 0I 1I