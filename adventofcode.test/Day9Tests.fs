module Day9Tests

open Xunit
open Day9

[<Fact>]
let test1 () =
    let input = "{}"
    let output = parse input
    let expected = [|GroupStart; GroupEnd|]
    (output = expected) |> Assert.True

[<Fact>]
let test2 () =
    let input = "{<>}"
    let output = parse input
    let expected = [| GroupStart; GarbageStart; GarbageEnd; GroupEnd |]
    (output = expected) |> Assert.True
    
[<Fact>]
let test3 () =
    let input = "{<!>dlajda>}"
    let output = parse input
    let expected = [| GroupStart; GarbageStart; GarbageEnd; GroupEnd |]
    (output = expected) |> Assert.True

[<Fact>]
let test4 () =
    let input = "{<{o\"i!a,<{i<a>}"
    let output = parse input
    let expected = [| GroupStart; GarbageStart; GarbageEnd; GroupEnd |]
    (output = expected) |> Assert.True

[<Fact>]
let test5 () =
    let output1 = parseAndScore "{}"
    (output1 = 1) |> Assert.True
    let output2 = parseAndScore "{{{}}}"
    (output2 = 6) |> Assert.True
    let output3 = parseAndScore "{{},{}}"
    (output3 = 5) |> Assert.True
    let output4 = parseAndScore "{{{},{},{{}}}}"
    (output4 = 16) |> Assert.True

    let output5 = parseAndScore "{<a>,<a>,<a>,<a>}"
    (output5 = 1) |> Assert.True

    let output6 = parseAndScore "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    (output6 = 9) |> Assert.True

    let output7 = parseAndScore "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    (output7 = 9) |> Assert.True

    let output8 = parseAndScore "{{<a!>},{<a!>},{<a!>},{<ab>}}"
    (output8 = 3) |> Assert.True