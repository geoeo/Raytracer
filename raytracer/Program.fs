open Raytracer.Scene
open BenchmarkDotNet.Running


let benchmarkScene = lazy BenchmarkRunner.Run<Raytracer.Scene.Scene>()

[<EntryPoint>]
let main argv =
    // let mainScene = new Scene ()
    // mainScene.renderScene ()
    // mainScene.saveFrameBuffer ()
    // mainScene.saveDepthBuffer ()
    let summary = benchmarkScene.Force()
    0 // return an integer exit code