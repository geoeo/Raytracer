open Raytracer.Scene

[<EntryPoint>]
let main argv =
    renderScene.Force()
    saveFrameBuffer.Force()
    saveDepthBuffer.Force()
    0 // return an integer exit code