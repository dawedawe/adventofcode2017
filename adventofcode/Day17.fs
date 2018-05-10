module Day17

open System.Collections.Generic

type BufferState = {
    buffer : List<int>
    position : int
    value : int
}

type BufferStatePart2 = {
    valueIndex1 : int
    position : int
    value : int
    bufferSize : int
}

let move (bufState : BufferState) steps =
    let newPos = (bufState.position + steps) % bufState.buffer.Count
    { bufState with position = newPos }

let movePart2 bufState steps =
    let newPos = (bufState.position + steps) % bufState.bufferSize
    { bufState with position = newPos }

let insertAfterCurrentPos (bufState : BufferState) =
    let newValue = bufState.value + 1
    let newPos = bufState.position + 1
    bufState.buffer.Insert(newPos, newValue)
    { bufState with value = newValue
                    position = newPos }

let insertAfterCurrentPosPart2 (bufState : BufferStatePart2) =
    let newValue = bufState.value + 1
    let newPos = bufState.position + 1
    let newValueIndex1 = if newPos = 1
                         then newValue
                         else bufState.valueIndex1
    let newSize = bufState.bufferSize + 1
    { bufState with value = newValue
                    position = newPos
                    bufferSize = newSize
                    valueIndex1 = newValueIndex1 }

let spin stepsToTake =
    let l : List<int> = List<int>()
    l.Insert(0, 0)
    let mutable bufState : BufferState = { buffer = l; position = 0; value = 0; }
    for _ in [1..2017] do
        bufState <- move bufState stepsToTake
        bufState <- insertAfterCurrentPos bufState
    bufState

let spinPart2 stepsToTake =
    let l : List<int> = List<int>()
    l.Insert(0, 0)
    let mutable bufState : BufferStatePart2 = { position = 0; value = 0; bufferSize = 1; valueIndex1 = 0}
    for _ in (Seq.init 50000000 id) do
        bufState <- movePart2 bufState stepsToTake
        bufState <- insertAfterCurrentPosPart2 bufState
    bufState

let day17 () =
    let b = spin 348
    let pos = b.position + 1
    b.buffer.[pos]

let day17Part2 () =
    let b = spinPart2 348
    b.valueIndex1