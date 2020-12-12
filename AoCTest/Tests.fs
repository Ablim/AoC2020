module Tests

open System.IO
open Xunit
open Validators
open CustomCustoms
open ColoredBags
open Boot
open XMAS
open Adapters
open Navigation

[<Theory>]
[<InlineData "Day5.txt">]
let day5Test filename =
    let data = File.ReadAllLines $"Input\\{filename}"
    let validate (row : string) =
        let parts = row.Split ' '
        Assert.True(isValidBoardingPass parts.[0] (parts.[1] |> int) (parts.[2] |> int) (parts.[3] |> int))
    Seq.iter validate data

[<Theory>]
[<InlineData ("Day6.txt", 11, 1)>]
[<InlineData ("Day6.txt", 6, 2)>]
let day6Test filename result part =
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
let day7Test filename result part =
    let data = Seq.toList (File.ReadAllLines $"Input\\{filename}")
    if part = 1 then
        let foundBags = bagSearchUp data
        Assert.Equal(result, foundBags.Length)
    else
        let sum = bagSearchDown data
        Assert.Equal(result, sum)

[<Theory>]
[<InlineData ("Day8.txt", 5, 1)>]
[<InlineData ("Day8.txt", 8, 2)>]
let day8Test filename result part =
    let data = File.ReadAllLines $"Input\\{filename}"
    if part = 1 then
        let value = execute data
        Assert.Equal(result, value.Accumulator)
    else
        let fixedAcc = executeAndFix data
        Assert.Equal(result, fixedAcc)

[<Theory>]
[<InlineData ("Day9.txt", "127", 1)>]
[<InlineData ("Day9.txt", "62", 2)>]
let day9Test filename result part =
    let data =
        File.ReadAllLines $"Input\\{filename}" |>
            Seq.map (fun x -> bigint.Parse x) |>
                Seq.toList
    if part = 1 then
        let value = hackerSearch data 5
        Assert.Equal(result, value.ToString())
    else
        let value = findSequence data 5
        Assert.Equal(result, value.ToString())

[<Theory>]
[<InlineData ("Day10.txt", "220", 1)>]
[<InlineData ("Day10.txt", "19208", 2)>]
[<InlineData ("Day10.1.txt", "8", 2)>]
let day10Test filename expected part =
    let input =
        File.ReadAllLines $"Input\\{filename}" |>
            Seq.map (fun x -> x |> int) |>
                Seq.toList
    if part = 1 then
        let output = connect input
        Assert.Equal(bigint.Parse expected, output)
    else
        let output = countCombinations input
        Assert.Equal(bigint.Parse expected, output)

[<Theory>]
[<InlineData ("Day12.txt", 25, 1)>]
[<InlineData ("Day12.txt", 0, 2)>]
let day12Test filename expected part =
    let input =
        File.ReadAllLines $"Input\\{filename}" |>
            Seq.toList
    if part = 1 then
        let output = getManhattanDist input
        Assert.Equal(expected, output)
    else
        //let output = countCombinations input
        //Assert.Equal(bigint.Parse expected, output)
        Assert.True(false)