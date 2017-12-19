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