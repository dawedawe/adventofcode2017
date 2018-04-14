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


    let doFirewallSteps2 (wall : Firewall) =
        let mutable caught = false
        let mutable i = 0
        while i < wall.layers.Length && not caught do
            caught <- (wall.layers.[i].scannerPosition.IsSome) && (wall.layers.[i].scannerPosition.Value = 0)
            let newLayers = Array.map doLayerStep wall.layers
            wall.layers <- newLayers
            i <- i + 1
        caught

    let sleep picoseconds wall =
        let mutable ls = Array.copy wall.layers
        for _ in [1 .. picoseconds] do
            ls <- Array.map doLayerStep ls
        { layers = ls }

    let day13 () =
        //let lines = System.IO.File.ReadAllLines "Day13Input.txt"
        let lines = [| "0: 3"; "1: 2"; "4: 4"; "6: 4" |]
        let wall = buildWall lines
        doFirewallSteps wall

    let day13Part2 () =
        let lines = System.IO.File.ReadAllLines "Day13Input.txt"
        // let lines = [| "0: 3"; "1: 2"; "4: 4"; "6: 4" |]
        let mutable caught = true
        let mutable wall = buildWall lines
        let mutable picoseconds = 0
        while caught do
            picoseconds <- picoseconds + 1
            wall <- sleep 1 wall
            caught <- doFirewallSteps2 { layers = Array.copy wall.layers }
        picoseconds
