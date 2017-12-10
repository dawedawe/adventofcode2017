module Day7

open System.Collections.Generic

let getLines path =
    System.IO.File.ReadAllLines path

let parse (line : string) =
    let firstSpaceIndex = line.IndexOf(" ")
    let supporter = line.Substring(0, firstSpaceIndex)
    let arrowIndex = line.IndexOf "->"
    let supported = if arrowIndex > 0
                    then
                        line.Substring(arrowIndex + 3).Split(',')
                        |> Array.map (fun x -> x.Trim())
                    else [||]
    supporter, supported

let day7 () =
    let lines = getLines "Day7Input.txt"
    let supporters = HashSet<string>()
    let supported = HashSet<string>()
    for l in lines do
        let sup, suped = parse l
        supporters.Add sup |> ignore
        Array.iter (supported.Add >> ignore) suped
    for s in supported do
        supporters.Remove s |> ignore
    supporters
