open Raytracer.Scene

[<EntryPoint>]
let main argv =
    renderSurfaces.Force()
    saveFrameBuffer.Force()
    saveDepthBuffer.Force()
    0 // return an integer exit code