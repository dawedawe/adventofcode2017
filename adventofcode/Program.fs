// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    // let input = [| 0; 3; 0; 1; -3 |]
    // let r = Day5.processOffsets 0 0 input
    let r = Day5.countStepsToExit ()
    printfn "steps %d" r
    0 // return an integer exit code
