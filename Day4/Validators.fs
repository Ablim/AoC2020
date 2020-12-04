module Validators

let safeParse i =
    try
        let value = i |> int
        Some value
    with
        | _ -> None

let isValidBirthYear year =
    match safeParse year with
    | Some y -> 1920 <= y && y <= 2002
    | None -> false

let isValidIssueYear year =
    match safeParse year with
    | Some y -> 2010 <= y && y <= 2020
    | None -> false

let isValidExpirationYear year =
    match safeParse year with
    | Some y -> 2020 <= y && y <= 2030
    | None -> false

let isValidHeight (height : string) =
    if height.EndsWith "cm" then
        let heightTrimmed = height.TrimEnd [| 'c'; 'm' |]
        let maybeHeight = safeParse heightTrimmed
        match maybeHeight with
        | Some value -> 150 <= value && value <= 193
        | None -> false
    elif height.EndsWith "in" then
        let heightTrimmed = height.TrimEnd [| 'i'; 'n' |]
        let maybeHeight = safeParse heightTrimmed
        match maybeHeight with
        | Some value -> 59 <= value && value <= 76
        | None -> false
    else
        false

let isValidHairColor (color : string) =
    if color.StartsWith "#" && color.Length = 7 then
        let values = Seq.toList color.[1..]
        let characters = List.append ['a'..'f'] ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
        Seq.map (fun x -> (Seq.contains x characters)) values |>
            Seq.contains false |>
                not
    else
        false

let isValidEyeColor color =
    match color with
    | "amb"
    | "blu"
    | "brn"
    | "gry" 
    | "grn"
    | "hzl" 
    | "oth" -> true
    | _ -> false

let isValidPassportId (passportId : string) =
    if passportId.Length = 9 then
        match safeParse passportId with
        | Some _ -> true
        | None -> false
    else
        false