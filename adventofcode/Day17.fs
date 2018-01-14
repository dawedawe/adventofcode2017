module Day17

open System.Collections.Generic

type BufferState = {
    buffer : List<int>
    position : int
    value : int
}

let move bufState steps =
    let newPos = (bufState.position + steps) % bufState.buffer.Count
    { bufState with position = newPos }

let insertAfterCurrentPos (bufState : BufferState) =
    let newValue = bufState.value + 1
    let newPos = bufState.position + 1
    bufState.buffer.Insert(newPos, newValue)
    { bufState with value = newValue
                    position = newPos }

let spin stepsToTake =
    let l : List<int> = List<int>()
    l.Insert(0, 0)
    let mutable bufState : BufferState = { buffer = l; position = 0; value = 0; }
    for i in [1..2017] do
        bufState <- move bufState stepsToTake
        bufState <- insertAfterCurrentPos bufState
    bufState

let day17 () =
    let b = spin 348
    let pos = b.position + 1
    b.buffer.[pos]