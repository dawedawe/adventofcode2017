module Day12

open System
open System.Linq
open System.Collections.Generic

let readInputFile path =
    System.IO.File.ReadAllLines path

let parseLine (line : string) =
    let arrowIndex = line.IndexOf("<->")
    let key = line.Substring(0, arrowIndex) |> Int32.Parse
    let values = line.Substring(arrowIndex + 3).Split(',')
                 |> Array.map Int32.Parse
    key, values

let constructDic lines =
    let dic = Dictionary<int, int array>()
    for line in lines do
        let pair = parseLine line
        dic.Add pair |> ignore
    dic

let getGroup (startId : int) (dic : Dictionary<int, int array>) : HashSet<int> =
    
    let rec getGroupHelper (dic : Dictionary<int, int array>) (idToFollow : int) (group : HashSet<int>) : HashSet<int> =
        let connectedIds = dic.[idToFollow] |> Array.filter (fun id -> id <> idToFollow)
                                            |> Array.filter (group.Contains >> not)
        group.UnionWith connectedIds
        for id in connectedIds do
            getGroupHelper dic id group |> ignore
        group
    let g = HashSet<int>()
    g.Add startId |> ignore
    getGroupHelper dic startId g
        
let day12 () =
    System.IO.File.ReadAllLines "Day12Input.txt"
    |> constructDic
    |> getGroup 0
    |> fun x -> x.Count

let parseLinePart2 (line : string) =
    let arrowIndex = line.IndexOf("<->")
    let key = line.Substring(0, arrowIndex) |> Int32.Parse
    let values = line.Substring(arrowIndex + 3).Split(',')
                 |> Array.map Int32.Parse
    Set.union (Set.singleton key) (Set.ofArray values)

let fintAffectedGroups existingGroups newGroup =
    let mutable affectedGroups = Set.empty
    for g in existingGroups do
        if Set.intersect g newGroup <> Set.empty
        then affectedGroups <- Set.add g affectedGroups
    affectedGroups

let day12Part2 () =
    let lines = System.IO.File.ReadAllLines "Day12Input.txt"
                |> Array.map parseLinePart2
    let mutable groups = Set.empty
    for lineGroup in lines do
        let affectedGroups = fintAffectedGroups groups lineGroup
        let mutable mergedGroup = Set.empty
        for aG in affectedGroups do
            groups <- Set.remove aG groups
            mergedGroup <- Set.union mergedGroup aG
        mergedGroup <- Set.union mergedGroup lineGroup
        groups <- Set.add mergedGroup groups
    Set.count groups