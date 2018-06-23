module Day24

type Component = {
    port1 : int
    port2 : int
}

type Bridge = Component List

let isComponentCompatible (bridge : Bridge) (comp : Component) =
    let lastCompPort2 = (List.last bridge).port2
    lastCompPort2 = comp.port1 || lastCompPort2 = comp.port2

let swapComponent comp =
    { port1 = comp.port2; port2 = comp.port1 }

let addComponentToBridge (bridge : Bridge) (comp : Component) =
    let lastCompPort2 = (List.last bridge).port2
    let isSwapNeeded = lastCompPort2 <> comp.port1 && lastCompPort2 = comp.port2
    let toAdd = if isSwapNeeded
                then swapComponent comp |> List.singleton
                else List.singleton comp
    let newBridge = List.append bridge toAdd
    newBridge, isSwapNeeded

let findStartComponents (components : Component array) =
    let swapStartIfNeeded startingComp =
        if (startingComp.port1 <> 0)
        then swapComponent startingComp
        else startingComp

    let startComponents = Array.filter (fun c -> c.port1 = 0 || c.port2 = 0) components
    let mutable startsAndRests = Set.empty
    for sC in startComponents do
        let rest = Set.difference (set(components)) (Set.singleton sC)
        let sC' = swapStartIfNeeded sC |> List.singleton
        let startAndRest = sC', rest
        startsAndRests <- Set.union startsAndRests  (Set.singleton startAndRest)
    startsAndRests

let rec findMatchingComponents (bridge : Bridge) (rest : Component Set) =
    let lastCompPort2 = (List.last bridge).port2
    let matchings = Set.filter (fun c -> c.port1 = lastCompPort2 || c.port2 = lastCompPort2) rest
    let mutable bridgesAndSwaps = Set.map (fun c -> addComponentToBridge bridge c) matchings
    let mutable bridges' = Set.empty
    for b, swapNeeded in bridgesAndSwaps do
        let last = if swapNeeded
                   then let lastC = List.last b
                        { port1 = lastC.port2; port2 = lastC.port1 } 
                   else List.last b
        let rest' = Set.remove last rest
        let newBridges = findMatchingComponents b rest'
        bridges' <- Set.union bridges' newBridges
    Set.union (Set.map fst bridgesAndSwaps) bridges' 

let parseLine (line : string) =
    let ports = line.Split('/')
    { port1 = int(ports.[0]); port2 = int(ports.[1]) }

let bridgeSum (bridge : Bridge) =
    List.sumBy (fun b -> b.port1 + b.port2) bridge

let bridgeLength (bridge : Bridge) =
    List.length bridge

[<Literal>]
let InputFile = "Day24Input.txt"

let constructAllPossibleBridges () =
    let input = System.IO.File.ReadAllLines InputFile
    let components = Array.map parseLine input
    let startingBridgesAndRests = findStartComponents components
    let mutable bridges = Set.map fst startingBridgesAndRests
    for sb, rest in startingBridgesAndRests do
        let bridges' = findMatchingComponents sb rest
        bridges <- Set.union bridges bridges'
    bridges

let day24 () =
    let bridges = constructAllPossibleBridges()
    let m = Set.maxElement(Set.map bridgeSum bridges)
    m

let day24Part2 () =
    let bridges = constructAllPossibleBridges()
    let longest = List.sortByDescending bridgeLength (Set.toList bridges)
    let longestLength = bridgeLength longest.[0]
    List.filter (fun b -> bridgeLength b = longestLength) longest
    |> List.sortByDescending bridgeSum
    |> List.item 0
    |> bridgeSum
    