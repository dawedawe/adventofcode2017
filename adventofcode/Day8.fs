module Day8

open System
open System.Collections.Generic
open System.Linq

type Register = {
    name : string
    mutable value : int
}

type Cpu = {
    registers : List<Register>
}

type Operation =
    | Inc
    | Dec

type Comparison =
    | Lt
    | Leq
    | Eq
    | Geq
    | Gt
    | Neq

type Condition = {
    register : string
    comparison : Comparison
    value : int
}

type Instruction = {
    register : string
    operation : Operation
    value : int
    condition : Condition
}

let parseOpeartion =
    function
    | "dec" -> Dec
    | "inc" -> Inc
    | _ -> raise(Exception("unknown operation"))

let parseComparison =
    function
    | "<" -> Lt
    | "<=" -> Leq
    | "==" -> Eq
    | ">=" -> Geq
    | ">" -> Gt
    | "!=" -> Neq
    | _ -> raise(Exception("unknown comparison"))

let parseCondition (line : string) =
    let regIndex = line.IndexOf(" if ") + 4
    let conditionString = line.Substring(regIndex)
    let words = conditionString.Split(' ')
    {register = words.[0]; comparison = parseComparison(words.[1]); value = int(words.[2])}

let parseLine (line : string) =
    let indexOfFirstSpace = line.IndexOf(" ")
    let reg = line.Substring(0, indexOfFirstSpace)
    let opString = line.Substring(indexOfFirstSpace + 1, 3)
    let op = parseOpeartion opString
    let indexOfValue = indexOfFirstSpace + 5
    let valX = line.Substring(indexOfValue).ToCharArray()
               |> Array.takeWhile (fun (c : char) -> Char.IsDigit c || c = '-')
               |> Array.fold (fun a c -> a + string(c)) ""
               |> int
    let cond = parseCondition line
    {
        register = reg
        operation = op
        value = valX
        condition = cond
    }

let getLines path =
    System.IO.File.ReadAllLines path

let createRegisterIfNeeded (cpu : Cpu) (registerName : string) =
    if not (cpu.registers.Any(fun r -> r.name = registerName))
    then cpu.registers.Add { name = registerName; value = 0 }
    ()

let evalConditon (cpu : Cpu) (cond : Condition) =
    let r = cpu.registers.Single(fun x -> x.name = cond.register)
    match cond.comparison with
    | Lt -> r.value < cond.value
    | Leq -> r.value <= cond.value
    | Eq -> r.value = cond.value
    | Geq -> r.value >= cond.value
    | Gt -> r.value > cond.value
    | Neq -> r.value <> cond.value

let evalOp (cpu : Cpu) (inst : Instruction) =
    let r = cpu.registers.Single(fun x -> x.name = inst.register)
    match inst.operation with
    | Inc -> r.value <- r.value + inst.value
    | Dec -> r.value <- r.value - inst.value

let eval (cpu : Cpu) (inst : Instruction) =
    createRegisterIfNeeded cpu inst.register
    createRegisterIfNeeded cpu inst.condition.register
    if evalConditon cpu inst.condition
    then evalOp cpu inst

let evalLines lines =
    let cpu = { registers = List<Register>() }
    for l in lines do
        let i = parseLine l
        eval cpu i |> ignore
    let m = cpu.registers.Select(fun r -> r.value).Max()
    m

let evalLinesPart2 lines =
    let cpu = { registers = List<Register>() }
    let mutable max = 0
    for l in lines do
        let i = parseLine l
        eval cpu i |> ignore
        let currentMax = cpu.registers.Select(fun r -> r.value).Max()
        if currentMax > max
        then max <- currentMax
    max
let day8 () =
    getLines "Day8Input.txt"
    |> evalLines
    
let day8Part2 () =
    getLines "Day8Input.txt"
    |> evalLinesPart2
