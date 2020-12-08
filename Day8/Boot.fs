module Boot

open Common.Common

type Summary =
    {
        History: int list
        Accumulator: int
        Inf: bool
    }

let apply (num:string) acc =
    let sign = num.[0]
    let num = parseInt num.[1..]

    match num with
    | Some i ->
        match sign with
        | '+' -> acc + i
        | '-' -> acc - i
        | _ -> acc
    | None -> acc

let execute (program:string[]) =
    let rec bootCheck (prog:string[]) pc acc hist =
        if pc >= prog.Length then
            { History = hist; Accumulator = acc; Inf = false; }
        elif Seq.contains pc hist then
            { History = hist; Accumulator = acc; Inf = true; }
        else
            let instructionParts = prog.[pc].Split " "
            let op = instructionParts.[0]
            let num = instructionParts.[1]
            match op with
            | "nop" -> bootCheck prog (pc+1) acc (pc::hist)
            | "acc" -> bootCheck prog (pc+1) (apply num acc) (pc::hist)
            | "jmp" -> bootCheck prog (apply num pc) acc (pc::hist)
            | _ -> { History = []; Accumulator = 0; Inf = false; }
    bootCheck program 0 0 []

let rec removeAccOps (program:string[]) (history: int list) =
    match history with
    | [] -> []
    | h::t ->
        if program.[h].Contains "acc" then
            removeAccOps program t
        else
            h::(removeAccOps program t)

let generateAndFlip (original:string[]) index =
    let flip (op:string) =
        let parts = op.Split " "
        match parts.[0] with
        | "nop" -> $"jmp {parts.[1]}"
        | "jmp" -> $"nop {parts.[1]}"
        | _ -> ""
    let newProgram = Array.copy original
    newProgram.[index] <- flip (original.[index])
    newProgram

let executeAndFix (program:string[]) =
    let firstRun = execute program
    printfn "Initial history length is %i" firstRun.History.Length
    let cleanHistory = removeAccOps program firstRun.History
    printfn "With no 'acc' history length is %i" cleanHistory.Length

    let alternatePrograms = Seq.map (generateAndFlip program) cleanHistory
    Seq.map (fun x ->
        let s = execute x
        if s.Inf then 0 else s.Accumulator)
        alternatePrograms |>
            Seq.sum