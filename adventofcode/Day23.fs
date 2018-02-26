module Day23

open System
open System.Collections.Generic

type Value =
    | IntVal of Int64
    | RegName of char

type Instruction =
    | Set of (char * Value)
    | Sub of (char * Value)
    | Mul of (char * Value)
    | Jnz of (Value * Value)

type Program = {
    registers : Dictionary<char, int64>
    instructions : Instruction array
    nextInstruction : Int64
    invokedMuls : int
}

let getValue (registers : Dictionary<char, int64>) (value : Value) =
    match value with
    | IntVal i -> i
    | RegName c -> registers.[c]

let getRegisters (value : Value) =
    match value with
    | IntVal _ -> None
    | RegName c -> Some c

let getTouchedRegs (instruction : Instruction) =
    match instruction with
    | Set (c,  value) -> [| Some c; getRegisters value |]
    | Sub (c, value) -> [| Some c; getRegisters value |]
    | Mul (c, value) -> [| Some c; getRegisters value |]
    | Jnz (val1, val2) -> [| getRegisters val1; getRegisters val2 |]
    |> Array.filter Option.isSome
    |> Array.map Option.get

let constructRegisters (instructions : Instruction array) =
    let dic = Dictionary<char, int64>()
    Array.map getTouchedRegs instructions
    |> Array.fold Array.append [||]
    |> Array.distinct
    |> Array.map (fun n -> dic.Add(n, 0L)) |> ignore
    dic

let constructProgram (instructions : Instruction array) : Program =
    let regs = constructRegisters instructions
    {
        instructions = instructions
        registers = regs
        nextInstruction = 0L
        invokedMuls = 0
    }

let progWithNextInstMovedBy n prog =
    { prog with nextInstruction = prog.nextInstruction + n }

let eval (prog : Program) : Program =
    match prog.instructions.[int(prog.nextInstruction)] with
    | Set (c,  value) -> let v = getValue prog.registers value
                         prog.registers.[c] <- v
                         progWithNextInstMovedBy 1L prog
    | Sub (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- prog.registers.[c] - v
                        progWithNextInstMovedBy 1L prog
    | Mul (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- prog.registers.[c] * v
                        progWithNextInstMovedBy 1L { prog with invokedMuls = prog.invokedMuls + 1 }
    | Jnz (val1, val2) -> let v1 = getValue prog.registers val1
                          let v2 = getValue prog.registers val2
                          if v1 <> 0L
                          then progWithNextInstMovedBy v2 prog
                          else progWithNextInstMovedBy 1L prog

let rec execute (prog : Program) =
    if prog.nextInstruction >= 0L && prog.nextInstruction < int64(prog.instructions.Length)
    then eval prog |> execute
    else prog.invokedMuls

let parseValue s =
    let r, i = Int64.TryParse(s)
    if r
    then IntVal i
    else RegName s.[0]

let parseInstruction (s : string) =
    let parts = s.Split [|' '|]
    match parts.[0] with
    | "set" -> Set (parts.[1].[0], (parseValue parts.[2]))
    | "sub" -> Sub (parts.[1].[0], (parseValue parts.[2]))
    | "mul" -> Mul (parts.[1].[0], (parseValue parts.[2]))
    | "jnz" -> Jnz ((parseValue parts.[1]), (parseValue parts.[2]))
    | _ -> raise(Exception("unknown instruction"))

let day23 () =
    let input = System.IO.File.ReadAllLines("Day23Input.txt")
    let instructions = Array.map parseInstruction input
    let prog = constructProgram instructions
    let r = execute prog
    r