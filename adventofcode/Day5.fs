module Day5

let getOffsets path =
    System.IO.File.ReadAllLines path
    |> Array.map int

let rec processOffsets steps currentIndex offsetManipulation (offsets : int array) =
    let newSteps = steps + 1
    let newIndex = currentIndex + offsets.[currentIndex]
    offsets.[currentIndex] <- offsetManipulation offsets.[currentIndex]
    if newIndex < 0 || newIndex >= offsets.Length
    then newSteps
    else processOffsets newSteps newIndex offsetManipulation offsets

let offsetManipulationPart1 offset = offset + 1

let offsetManipulationPart2 offset =
    if offset >= 3
    then offset - 1
    else offset + 1

let countStepsToExit () =
    getOffsets "Day5Input.txt"
    |> processOffsets 0 0 offsetManipulationPart1

let countStepsToExitPart2 () =
    getOffsets "Day5Input.txt"
    |> processOffsets 0 0 offsetManipulationPart2
