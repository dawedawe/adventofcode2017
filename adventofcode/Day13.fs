module Day13
// first packet, then scanner

type Direction =
    | Down
    | Up

type Layer = {
    depth : int
    range : int
    scannerPosition : int option
    scannerDirection : Direction option
}

type Firewall = {
    mutable layers : Layer array
}

let parseLine (line : string) : Layer =
    let parts = line.Split ':'
    let d = int(parts.[0])
    let r = int(parts.[1])
    { depth = d; range = r; scannerPosition = Some 0; scannerDirection = Some Down }

let buildWall lines =
    let layers = Array.map parseLine lines
    let wallLayersCount = (Array.last layers).depth + 1
    let wall = Array.init wallLayersCount (fun x -> { depth = x; range = 0; scannerPosition = None; scannerDirection = None })
    for layer in layers do
        wall.[layer.depth] <- layer
    { layers = wall }

let doLayerStep (layer : Layer) =
    if layer.range <= 1
    then layer
    else
        let newDir = if (layer.scannerDirection.Value = Down)
                     then
                        if layer.scannerPosition.Value < (layer.range - 1)
                        then Down
                        else Up
                     else
                        if layer.scannerPosition.Value > 0
                        then Up
                        else Down
        let newPos = if newDir = Down
                     then layer.scannerPosition.Value + 1
                     else layer.scannerPosition.Value - 1
        { layer with scannerPosition = Some newPos; scannerDirection = Some newDir }

let doFirewallSteps (wall : Firewall) =
    let mutable sum = 0
    for i in [0 .. (wall.layers.Length - 1)] do
        if (wall.layers.[i].scannerPosition.IsSome) && (wall.layers.[i].scannerPosition.Value = 0)
        then sum <- sum + (wall.layers.[i].depth * wall.layers.[i].range)
        let newLayers = Array.map doLayerStep wall.layers
        wall.layers <- newLayers
    sum

let day13 () =
    let lines = System.IO.File.ReadAllLines "Day13Input.txt"
    let wall = buildWall lines
    doFirewallSteps wall