module XMAS

open Common.Common

let private isSum (nums:bigint list) (num:bigint) =
    let max = Seq.max nums
    if num >= max * 2I then
        false
    else
        let candidates =
            seq {
                for i in nums do
                    for j in nums do
                        yield i + j
            } |>
                Seq.toList
        Seq.contains num candidates

let hackerSearch (data:bigint list) preambleSize =
    let rec loop (body:bigint list) (preamble:bigint list) =
        match body with
        | [] -> 0I
        | h::t ->
            if isSum preamble h then
                loop t (addLast preamble.Tail h)
            else
                h
    
    let preamble = data.[..(preambleSize-1)]
    let body = data.[preambleSize..]
    loop body preamble

let private getMinMaxSum (sequence: bigint list) =
    match sequence with
    | [] -> 0I
    | _ -> (Seq.min sequence) + (Seq.max sequence)

let findSequence (data: bigint list) preambleSize =
    let invalidNum = hackerSearch data preambleSize
    let rec loop (numbers: bigint list) (index: int) (sequence: bigint list) =
        let currentSum = Seq.sum sequence

        if currentSum = invalidNum then
            getMinMaxSum sequence
        elif currentSum < invalidNum then
            match numbers with
            | [] -> 0I
            | h::t -> loop t index (h::sequence)
        else
            loop data.[index..] (index+1) []

    loop data 1 []