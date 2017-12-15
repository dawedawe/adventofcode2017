module Day10Tests

open Xunit
open System.ComponentModel.DataAnnotations.Schema

[<Fact>]
let test1 () =
    let input = [| 0; 1; 2; 3; 4; |]
    let output = Day10.replace input 3 [| 4; 3; 2; 1; 0|]
    let expected = [| 2; 1; 0; 4; 3 |]
    (expected = output) |> Assert.True

[<Fact>]
let test2 () =
    let input = [| 0; 1; 2; 3; 4; |]
    let output = Day10.reverse input 2 0
    let expected = [| 1; 0; 2; 3; 4; |]
    (expected = output) |> Assert.True


[<Fact>]
let test3 () =
    let input = [| 0; 1; 2; 3; 4; |]
    let output = Day10.reverse input 2 4
    let expected = [| 4; 1; 2; 3; 0 |]
    (expected = output) |> Assert.True

[<Fact>]
let test4 () =
    let input = [| 0; 1; 2; 3; 4; |]
    let output = Day10.replace input 4 [|3; 2; 1; 0; 4|]
    let expected = [| 2; 1; 0; 4; 3 |]
    (expected = output) |> Assert.True


[<Fact>]
let test5 () =
    let input = [| 0; 1; 2; 3; 4; |]
    let lengths = [| 3; 4; 1; 5 |]
    let output = Day10.day10helper input lengths
    let expected = [| 3; 4; 2; 1; 0 |]
    (expected = output) |> Assert.True