module Day5

let getOffsets path =
    System.IO.File.ReadAllLines path
    |> Array.map int

let rec processOffsets steps currentIndex (offsets : int array) =
    let newSteps = steps + 1
    let newIndex = currentIndex + offsets.[currentIndex]
    offsets.[currentIndex] <- offsets.[currentIndex] + 1 
    if newIndex < 0 || newIndex >= offsets.Length
    then newSteps
    else processOffsets newSteps newIndex offsets

let countStepsToExit () =
    getOffsets "Day5Input.txt"
    |> processOffsets 0 0

