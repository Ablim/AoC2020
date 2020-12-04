open System.IO
open Validators

let getLength (x : 'a list) = x.Length

let splitPassports data =
    let rec loop data current (all : string list) =
        match data with
        | [] -> current :: all
        | head :: tail ->
            match head with
            | "" -> loop tail "" (current :: all)
            | line -> loop tail (current + " " + line) all
    let passports = loop data "" []
    printfn "Total passports %i" passports.Length
    passports

let isValid (password : string) =
    let checkPart (part : string) =
        match part.[..2] with
        | "byr" -> 1
        | "iyr" -> 1
        | "eyr" -> 1
        | "hgt" -> 1
        | "hcl" -> 1
        | "ecl" -> 1
        | "pid" -> 1
        | _ -> 0
    let parts = password.Split ' '
    Seq.sum (Seq.map checkPart parts) = 7

let countValidPassports data =
    (Seq.toList (Seq.where (fun x -> x) (Seq.map (fun x -> isValid x ) data))).Length

let isValid2 (password : string) =
    let checkPart (part : string) =
        match part.[..2] with
        | "byr" -> isValidBirthYear part.[4..]
        | "iyr" -> isValidIssueYear part.[4..]
        | "eyr" -> isValidExpirationYear part.[4..]
        | "hgt" -> isValidHeight part.[4..]
        | "hcl" -> isValidHairColor part.[4..]
        | "ecl" -> isValidEyeColor part.[4..]
        | "pid" -> isValidPassportId part.[4..]
        | _ -> false
    let parts = password.Split ' '
    let oks =
        Seq.map checkPart parts |>
            Seq.where (fun x -> x) |>
                Seq.toList |>
                    getLength
    oks = 7

let countValidPassports2 data =
    Seq.map (fun x -> isValid2 x ) data |>
        Seq.where (fun x -> x) |>
            Seq.toList |>
                getLength

[<EntryPoint>]
let main argv =
    let input = Seq.toList (File.ReadAllLines "Input.txt")
    let passports = splitPassports input
    
    // Part 1
    printfn "Answer to part 1 is %i" (countValidPassports passports)

    // Part 2
    printfn "Answer to part 2 is %i" (countValidPassports2 passports)

    //let valid = Seq.toList (File.ReadAllLines "Valid.txt")
    //let validPassports = splitPassports valid
    //printfn "Valid %i" (countValidPassports2 validPassports)

    //let invalid = Seq.toList (File.ReadAllLines "Invalid.txt")
    //let invalidPassports = splitPassports invalid
    //printfn "Invalid %i" (countValidPassports2 invalidPassports)

    0 // return an integer exit code
