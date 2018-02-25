// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let r = Day21.day21 ()
    printfn "%A" r
    0 // return an integer exit code
