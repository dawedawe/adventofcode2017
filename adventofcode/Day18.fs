module Day18

open System
open System.Collections.Generic

type Value =
    | IntVal of Int64
    | RegName of char

type State =
    | Running
    | Waiting
    | Finished

type Instruction =
    | Snd of Value
    | SndPart2 of Value
    | Set of (char * Value)
    | Add of (char * Value)
    | Mul of (char * Value)
    | Mod of (char * Value)
    | Rcv of Value
    | RcvPart2 of Value
    | Jgz of (Value * Value)

type Program = {
    registers : Dictionary<char, int64>
    instructions : Instruction array
    nextInstruction : Int64
    lastPlayedSound : Int64 option
    recovered : bool
}

type ProgramPart2 = {
    id : Int64
    registers : Dictionary<char, int64>
    instructions : Instruction array
    nextInstruction : Int64
    state : State
    valuesSendCount : Int64
    inQueue : System.Collections.Generic.Queue<Int64>
    mutable outQueue : System.Collections.Generic.Queue<Int64>
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
    | SndPart2 value -> [| getRegisters value |]
    | Set (c,  value) -> [| Some c; getRegisters value |]
    | Add (c, value) -> [| Some c; getRegisters value |]
    | Mul (c, value) -> [| Some c; getRegisters value |]
    | Mod (c, value) -> [| Some c; getRegisters value |]
    | Rcv value -> [| getRegisters value |]
    | RcvPart2 value -> [| getRegisters value |]
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
    
let constructProgramPart2 (instructions : Instruction array) id : ProgramPart2 =
    let regs = constructRegisters instructions
    regs.['p'] <- id
    let program = {
        id = id
        instructions = instructions
        registers = regs
        nextInstruction = 0L
        state = Running
        valuesSendCount = 0L
        inQueue = System.Collections.Generic.Queue<Int64>()
        outQueue = System.Collections.Generic.Queue<Int64>()
    }
    program

let progWithNextInstMovedBy n prog : Program =
    { prog with nextInstruction = prog.nextInstruction + n }

let progWithNextInstMovedByPart2 n prog : ProgramPart2 =
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
    | _ -> raise(Exception("unknown instruction"))

let evalPart2 (prog : ProgramPart2) : ProgramPart2 =
    match prog.instructions.[int(prog.nextInstruction)] with
    | SndPart2 value -> let v = getValue prog.registers value
                        prog.outQueue.Enqueue v
                        progWithNextInstMovedByPart2 1L { prog with valuesSendCount = prog.valuesSendCount + 1L }
    | Set (c,  value) -> let v = getValue prog.registers value
                         prog.registers.[c] <- v
                         progWithNextInstMovedByPart2 1L prog
    | Add (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- prog.registers.[c] + v
                        progWithNextInstMovedByPart2 1L prog
    | Mul (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- prog.registers.[c] * v
                        progWithNextInstMovedByPart2 1L prog
    | Mod (c, value) -> let v = getValue prog.registers value
                        prog.registers.[c] <- (prog.registers.[c] % v)
                        progWithNextInstMovedByPart2 1L prog
    | RcvPart2 value -> let r = getRegisters value
                        if (prog.inQueue.Count > 0)
                        then
                            let v = prog.inQueue.Dequeue()
                            prog.registers.[r.Value] <- v
                            progWithNextInstMovedByPart2 1L { prog with state = Running }
                        else
                            { prog with state = Waiting }
    | Jgz (val1, val2) -> let v1 = getValue prog.registers val1
                          let v2 = getValue prog.registers val2
                          if v1 > 0L
                          then progWithNextInstMovedByPart2 v2 prog
                          else progWithNextInstMovedByPart2 1L prog
    | _ -> raise(Exception("unknown instruction"))


let rec execute (prog : Program) =
    if prog.recovered
    then prog.lastPlayedSound.Value
    else eval prog |> execute

let rec executePart2 (prog : ProgramPart2) =
    if prog.nextInstruction >= int64(prog.instructions.Length) || prog.nextInstruction < 0L
    then { prog with state = State.Finished }
    else evalPart2 prog

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

    
let parseInstructionPart2 (s : string) =
    let parts = s.Split [|' '|]
    match parts.[0] with
    | "snd" -> SndPart2 (parseValue parts.[1])
    | "set" -> Set (parts.[1].[0], (parseValue parts.[2]))
    | "add" -> Add (parts.[1].[0], (parseValue parts.[2]))
    | "mul" -> Mul (parts.[1].[0], (parseValue parts.[2]))
    | "mod" -> Mod (parts.[1].[0], (parseValue parts.[2]))
    | "rcv" -> RcvPart2 (parseValue parts.[1])
    | "jgz" -> Jgz ((parseValue parts.[1]), (parseValue parts.[2]))
    | _ -> raise(Exception("unknown instruction"))

let programsInDeadlockOrFinished p0 p1 =
    (p0.state = Waiting && p1.state = Waiting)
    ||
    (p0.state = Finished && p1.state = Finished)

[<Literal>]
let InputFile = "Day18Input.txt"

let day18 () =
    let input = System.IO.File.ReadAllLines(InputFile)
    let instructions = Array.map parseInstruction input
    let prog = constructProgram instructions
    let r = execute prog
    r

let day18Part2 () =
    let input = System.IO.File.ReadAllLines(InputFile)
    let instructions = Array.map parseInstructionPart2 input
    let mutable prog0 = constructProgramPart2 instructions 0L
    let mutable prog1 = constructProgramPart2 instructions 1L
    prog0.outQueue <- prog1.inQueue
    prog1.outQueue <- prog0.inQueue
    while (not (programsInDeadlockOrFinished prog0 prog1)) do
        prog0 <- executePart2 prog0
        prog1 <- executePart2 prog1
    prog1.valuesSendCount
