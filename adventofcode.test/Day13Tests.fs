module Day13Tests

open Xunit
open Day13

[<Fact>]
let test1 () =
    let layer = { depth = 0; range = 2; scannerPosition = Some 0; scannerDirection = Some Down }
    let layerAfterStep1 = doLayerStep layer
    Assert.True ((layerAfterStep1.scannerDirection.Value = Down))
    Assert.True ((layerAfterStep1.scannerPosition.Value = 1))
    let layerAfterStep2 = doLayerStep layerAfterStep1
    Assert.True ((layerAfterStep2.scannerDirection = Some Up))
    Assert.True ((layerAfterStep2.scannerPosition = Some 0))
    let layerAfterStep3 = doLayerStep layerAfterStep2
    Assert.True ((layerAfterStep3.scannerDirection.Value = Down))
    Assert.True ((layerAfterStep3.scannerPosition.Value = 1))
    let layerAfterStep4 = doLayerStep layerAfterStep3
    Assert.True ((layerAfterStep4.scannerDirection = Some Up))
    Assert.True ((layerAfterStep4.scannerPosition = Some 0))
    