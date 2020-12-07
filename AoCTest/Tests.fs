module Tests

open System.IO
open Xunit
open Validators
open CustomCustoms
open ColoredBags

[<Theory>]
[<InlineData "Day5.txt">]
let ``Day5Test`` (filename) =
    let data = File.ReadAllLines $"Input\\{filename}"
    let validate (row : string) =
        let parts = row.Split ' '
        Assert.True(isValidBoardingPass parts.[0] (parts.[1] |> int) (parts.[2] |> int) (parts.[3] |> int))
    Seq.iter validate data

[<Theory>]
[<InlineData ("Day6.txt", 11, 1)>]
[<InlineData ("Day6.txt", 6, 2)>]
let ``Day6Test`` (filename, result, part) =
    let data = Seq.toList (File.ReadAllLines $"Input\\{filename}")
    if part = 1 then
        let sum = getQuestionSum data
        Assert.Equal(result, sum)
    else
        let sum = getQuestionSum2 data
        Assert.Equal(result, sum)

[<Theory>]
[<InlineData ("Day7.txt", 4, 1)>]
[<InlineData ("Day7.txt", 32, 2)>]
let ``Day7Test`` (filename, result, part) =
    let data = Seq.toList (File.ReadAllLines $"Input\\{filename}")
    if part = 1 then
        let foundBags = bagSearchUp data
        Assert.Equal(result, foundBags.Length)
    else
        //bagSearchDown
        Assert.True(false)