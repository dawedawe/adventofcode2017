module Day7

open System.Collections.Generic
open System.Linq

type Program = {
    name : string
    weight : int
}

type Tower = {
    rootProgram : Program
    subTowers : string array
}

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
    List(supporters).[0]

let parseTowerLine (line : string) =
    let firstSpaceIndex = line.IndexOf(" ")
    let supporter = line.Substring(0, firstSpaceIndex)
    let openingBracket = line.IndexOf("(")
    let closingBracket = line.IndexOf(")")
    let supporterWeight = line.Substring(openingBracket + 1, closingBracket - openingBracket - 1) |> int
    let arrowIndex = line.IndexOf "->"
    let supported = if arrowIndex > 0
                    then
                        line.Substring(arrowIndex + 3).Split(',')
                        |> Array.map (fun x -> x.Trim())
                    else [||]
    let rootProg = { name = supporter; weight = supporterWeight }
    { rootProgram = rootProg;
      subTowers = supported}

let rec calcSubTowerWeight (root : string) (towers : Dictionary<string, Tower>) =
    let rootWeight = towers.[root].rootProgram.weight
    let subTowers = towers.[root].subTowers
    let subWeights = Array.map (fun t -> calcSubTowerWeight t towers) subTowers
    rootWeight + Array.sum subWeights

let findOffBalance (subTowersAndWeight : (string * int) []) = 
    let counts = Array.countBy snd subTowersAndWeight
    let offBalanceWeight = Array.filter (fun c -> snd c = 1) counts
    if (offBalanceWeight.Length = 0)
    then None
    else
        let oW = fst offBalanceWeight.[0]
        subTowersAndWeight.First (fun s -> snd s = oW) |> fst |> Some

let identifyOffBalanceProgram (d : Dictionary<string, Tower>) =
    let mutable root = day7 ()
    let subTowersAndWeight = Array.map (fun s -> s, calcSubTowerWeight s d) d.[root].subTowers
    let mutable offBalanceTower = findOffBalance subTowersAndWeight
    while offBalanceTower.IsSome do
        root <- offBalanceTower.Value
        let subTowersAndWeight = Array.map (fun s -> s, calcSubTowerWeight s d) d.[root].subTowers
        offBalanceTower <- findOffBalance subTowersAndWeight
    root

let day7Part2 () =
    let lines = getLines "Day7Input.txt"
    let d = Dictionary<string, Tower>()
    Array.iter (fun l -> let t = parseTowerLine l
                         d.Add(t.rootProgram.name, t)) lines
    let offProgram = identifyOffBalanceProgram d
    let supporter = d.First(fun p -> p.Value.subTowers.Contains(offProgram))
    let subTowersAndWeight = Array.map (fun s -> s, calcSubTowerWeight s d) d.[supporter.Key].subTowers
    let normalWeight = snd <| subTowersAndWeight.First(fun s -> fst s <> offProgram)
    let abnormalWeight = snd <| subTowersAndWeight.First(fun s -> fst s = offProgram)
    let diff = normalWeight - abnormalWeight
    d.[offProgram].rootProgram.weight + diff