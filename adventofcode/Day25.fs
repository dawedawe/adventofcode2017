module Day25

open System
open System.Linq
open System.Collections.Generic

type Movement =
    | Left
    | Right

type Transition = {
    condition : int -> bool
    value : int
    move : Movement
    state : string
}

type State = {
    name : string
    transitions : Transition list
}

let StateA : State =
    let trans1 = {
        condition = fun x -> x = 0
        value = 1
        move = Right
        state = "B"
    }
    let trans2 = {
        condition = fun x -> x = 1
        value = 0
        move = Right
        state = "F"
    }
    { name = "A"; transitions = [ trans1; trans2 ] }
 
let StateB : State =
    let trans1 = {
        condition = fun x -> x = 0
        value = 0
        move = Left
        state = "B"
    }
    let trans2 = {
        condition = fun x -> x = 1
        value = 1
        move = Left
        state = "C"
    }
    { name = "B"; transitions = [ trans1; trans2 ] }

let StateC : State =
    let trans1 = {
        condition = fun x -> x = 0
        value = 1
        move = Left
        state = "D"
    }
    let trans2 = {
        condition = fun x -> x = 1
        value = 0
        move = Right
        state = "C"
    }
    { name = "C"; transitions = [ trans1; trans2 ] }

let StateD : State =
    let trans1 = {
        condition = fun x -> x = 0
        value = 1
        move = Left
        state = "E"
    }
    let trans2 = {
        condition = fun x -> x = 1
        value = 1
        move = Right
        state = "A"
    }
    { name = "D"; transitions = [ trans1; trans2 ] }

let StateE : State =
    let trans1 = {
        condition = fun x -> x = 0
        value = 1
        move = Left
        state = "F"
    }
    let trans2 = {
        condition = fun x -> x = 1
        value = 0
        move = Left
        state = "D"
    }
    { name = "E"; transitions = [ trans1; trans2 ] }

let StateF : State =
    let trans1 = {
        condition = fun x -> x = 0
        value = 1
        move = Right
        state = "A"
    }
    let trans2 = {
        condition = fun x -> x = 1
        value = 0
        move = Left
        state = "E"
    }
    { name = "F"; transitions = [ trans1; trans2 ] }

[<Literal>]
let Steps = 12425180

let states : Dictionary<string, State> =
    let d = Dictionary<string, State>()
    d.["A"] <- StateA
    d.["B"] <- StateB
    d.["C"] <- StateC
    d.["D"] <- StateD
    d.["E"] <- StateE
    d.["F"] <- StateF
    d

let operate (band : Dictionary<int, int>) (currentState : State) (currentPosition : int) =
    let currentValue = band.[currentPosition]
    let trans = List.filter (fun t -> t.condition currentValue) currentState.transitions
    if (List.length trans <> 1)
    then raise(Exception("non-determinism detected"))
    let t = trans.[0]
    band.[currentPosition] <- t.value
    let currentPosition' = if t.move = Left
                           then currentPosition - 1
                           else currentPosition + 1
    if (not (band.ContainsKey currentPosition'))
    then band.[currentPosition'] <- 0
    (currentPosition', t.state)

let day25 () =
    let mutable band = Dictionary<int, int>()
    let mutable currentPosition = 0
    let mutable currentState = StateA
    band.[currentPosition] <- 0
    for _ in [1 .. Steps] do
        let position, state = operate band currentState currentPosition
        currentPosition <- position
        currentState <- states.[state]
    band.Values.Sum()
