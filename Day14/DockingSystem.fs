module DockingSystem

let private parseMask (row: string) =
    row.Split(" ")
        .[2] |>
            Seq.rev |>
                Seq.toArray

let private parseValue (row: string) =
    row.Split(" ")
        .[2] |>
            bigint.Parse

let private parseAddress (row: string) =
    row.Split(" ")
        .[0]
            .Replace("mem[", "")
                .Replace("]", "") |>
                    bigint.Parse

let private to36Bit value =
    let rec loop (array: char []) index remaining =
        if index < 0 then
            array
        else
            let currentBit = pown 2I index
            if remaining >= currentBit then
                array.[index] <- '1'
                loop array (index-1) (remaining-currentBit)
            else
                array.[index] <- '0'
                loop array (index-1) remaining
    loop (Array.zeroCreate<char> 36) 35 value

let private from36Bit (value: char []) =
    seq {
        for i in 0..35 do
            let bit = bigint.Parse (value.[i].ToString())
            bit * (pown 2I i)
    } |>
        Seq.sum

let private applyMask value mask =
    let rec loop (oldVal: char []) (mask: char []) (newVal: char []) i =
        if i < 0 then
            newVal
        else
            newVal.[i] <- if mask.[i] = 'X' then oldVal.[i] else mask.[i]
            loop oldVal mask newVal (i-1)
    let oldValue = to36Bit value
    loop oldValue mask (Array.zeroCreate<char> 36) 35 |>
        from36Bit

let private applyFloatingMask value mask =
    let rec loop (oldVal: char []) (mask: char []) (newVal: char []) i =
        if i < 0 then
            newVal
        else
            newVal.[i] <- if mask.[i] = '0' then oldVal.[i] else mask.[i]
            loop oldVal mask newVal (i-1)
    let oldValue = to36Bit value
    loop oldValue mask (Array.zeroCreate<char> 36) 35

let initializeMemory input =
    let rec loop (input: string list) (memory: Map<bigint, bigint>) mask =
        match input with
        | [] -> memory
        | h::t ->
            match h.[..2] with
            | "mas" ->
                let newMask = parseMask h
                loop t memory newMask
            | "mem" ->
                let value = parseValue h
                let maskedValue = applyMask value mask
                let address = parseAddress h
                loop t (memory.Add(address, maskedValue)) mask
            | s -> failwithf "Invalid data '%s'" s
    loop input Map.empty [||]

let private maskAddress address mask =
    let rec generate (toProcess: seq<char []>) i =
        if i < 0 then
            toProcess
        else
            let newToProcess =
                seq {
                    for p in toProcess do
                        if p.[i] = 'X' then
                            let copyOne = Array.copy p
                            copyOne.[i] <- '1'
                            let copyZero = Array.copy p
                            copyZero.[i] <- '0'
                            [ copyOne; copyZero ]
                        else
                            [p]
                } |>
                    Seq.concat
            generate newToProcess (i-1)
    let addressWithFloating = applyFloatingMask address mask
    generate [addressWithFloating] 35

let rec private writeMany (memory: Map<'a, 'b>) addresses value =
    match Seq.tryHead addresses with
    | None -> memory
    | Some a -> writeMany (memory.Add(a, value)) (Seq.tail addresses) value

let initializeMemory2 input =
    let rec loop (input: string list) (memory: Map<bigint, bigint>) mask =
        match input with
        | [] -> memory
        | h::t ->
            match h.[..2] with
            | "mas" ->
                let newMask = parseMask h
                loop t memory newMask
            | "mem" ->
                let address = parseAddress h
                let addresses =
                    maskAddress address mask |>
                        Seq.map (fun x -> from36Bit x)
                let value = parseValue h
                let newMemory = writeMany memory addresses value
                loop t newMemory mask
            | s -> failwithf "Invalid data '%s'" s
    loop input Map.empty [||]

let sumMemory (input: Map<bigint, bigint>) =
    Seq.map (fun x -> x) input |>
        Seq.map (fun x -> x.Value) |>
            Seq.sum