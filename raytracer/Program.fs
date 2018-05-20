open Raytracer.Scene

[<EntryPoint>]
let main argv =
    renderScene
    saveFrameBuffer
    saveDepthBuffer
    0 // return an integer exit code