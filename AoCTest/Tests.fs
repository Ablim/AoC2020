module Tests

open System.IO
open Xunit
open Validators

[<Theory>]
[<InlineData "Day5.txt">]
let ``Day5Test`` (filename) =
    let data = File.ReadAllLines $"Input\\{filename}"
    let validate (row : string) =
        let parts = row.Split ' '
        Assert.True(isValidBoardingPass parts.[0] (parts.[1] |> int) (parts.[2] |> int) (parts.[3] |> int))
    Seq.iter validate data
