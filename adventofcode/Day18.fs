module Day18

open System
open System.Collections.Generic

type Value =
    | IntVal of Int64
    | RegName of char

type Instruction =
    | Snd of Value
    | Set of (char * Value)
    | Add of (char * Value)
    | Mul of (char * Value)
    | Mod of (char * Value)
    | Rcv of Value
    | Jgz of (Value * Value)

type Program = {
    registers : Dictionary<char, int64>
    instructions : Instruction array
    nextInstruction : Int64
    lastPlayedSound : Int64 option
    recovered : bool
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
    | Snd value -> [| getRegisters value |]
    | Set (c,  value) -> [| Some c; getRegisters value |]
    | Add (c, value) -> [| Some c; getRegisters value |]
    | Mul (c, value) -> [| Some c; getRegisters value |]
    | Mod (c, value) -> [| Some c; getRegisters value |]
    | Rcv value -> [| getRegisters value |]
    | Jgz (val1, val2) -> [| getRegisters val1; getRegisters val2 |]
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
        lastPlayedSound = None
        recovered = false
    }

let progWithNextInstMovedBy n prog =
    { prog with nextInstruction = prog.nextInstruction + n }

let eval (prog : Program) : Program =
    match prog.instructions.[int(prog.nextInstruction)] with
    | Snd value -> let v = getValue prog.registers value
                   progWithNextInstMovedBy 1L { prog with lastPlayedSound = Some v; }
    | Set (c,  value) -> let v = getValue prog.registers value
                         prog.registers.[c] <- v
                         progWithNextInstMovedBy 1L prog
    | Add (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- prog.registers.[c] + v
                        progWithNextInstMovedBy 1L prog
    | Mul (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- prog.registers.[c] * v
                        progWithNextInstMovedBy 1L prog
    | Mod (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- (prog.registers.[c] % v)
                        progWithNextInstMovedBy 1L prog
    | Rcv value -> let v = getValue prog.registers value
                   if v <> 0L && prog.lastPlayedSound.IsSome
                   then progWithNextInstMovedBy 1L { prog with recovered = true }
                   else progWithNextInstMovedBy 1L prog
    | Jgz (val1, val2) -> let v1 = getValue prog.registers val1
                          let v2 = getValue prog.registers val2
                          if v1 > 0L
                          then progWithNextInstMovedBy v2 prog
                          else progWithNextInstMovedBy 1L prog

let rec execute (prog : Program) =
    if prog.recovered
    then prog.lastPlayedSound.Value
    else eval prog |> execute

let parseValue s =
    let r, i = Int64.TryParse(s)
    if r
    then IntVal i
    else RegName s.[0]

let parseInstruction (s : string) =
    let parts = s.Split [|' '|]
    match parts.[0] with
    | "snd" -> Snd (parseValue parts.[1])
    | "set" -> Set (parts.[1].[0], (parseValue parts.[2]))
    | "add" -> Add (parts.[1].[0], (parseValue parts.[2]))
    | "mul" -> Mul (parts.[1].[0], (parseValue parts.[2]))
    | "mod" -> Mod (parts.[1].[0], (parseValue parts.[2]))
    | "rcv" -> Rcv (parseValue parts.[1])
    | "jgz" -> Jgz ((parseValue parts.[1]), (parseValue parts.[2]))
    | _ -> raise(Exception("unknown instruction"))

let day18 () =
    let input = System.IO.File.ReadAllLines("Day18Input.txt")
    let instructions = Array.map parseInstruction input
    let prog = constructProgram instructions
    let r = execute prog
    r