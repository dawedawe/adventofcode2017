module Day11Tests

open Xunit
open Day11

[<Fact>]
let test1 () =
    let input = [ NE; NE; NE ] 
    let pos = { x = 0; y = 0; z = 0 }
    let output = go pos input
    let dist = distance pos output
    (dist = 3)|> Assert.True

[<Fact>]
let test2 () =
    let input = [ NE; NE; SW; SW ] 
    let pos = { x = 0; y = 0; z = 0 }
    let output = go pos input
    let dist = distance pos output
    (dist = 0) |> Assert.True

[<Fact>]
let test3 () =
    let input = [ NE; NE; S; S ] 
    let pos = { x = 0; y = 0; z = 0 }
    let output = go pos input
    let dist = distance pos output
    (dist = 2) |> Assert.True

[<Fact>]
let test4 () =
    let input = [ SE; SW; SE; SW; SW ] 
    let pos = { x = 0; y = 0; z = 0 }
    let output = go pos input
    let dist = distance pos output
    (dist = 3) |> Assert.True