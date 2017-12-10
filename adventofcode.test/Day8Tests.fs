module Day8Tests

open Xunit
open Day8
open System.Collections.Generic

[<Fact>]
let createRegIfNeededTest () =
    let cpu = { registers = List<Register>() }
    Day8.createRegisterIfNeeded cpu "r1" |> ignore
    Assert.True(1 = cpu.registers.Count)
    Assert.True(cpu.registers.[0].name = "r1")
    Assert.True(cpu.registers.[0].value = 0)

[<Fact>]
let test1 () =
    let input = ["b inc 5 if a > 1";
                "a inc 1 if b < 5";
                "c dec -10 if a >= 1";
                "c inc -20 if c == 10"]
    let r = Day8.evalLines input
    Assert.True(1 = r)