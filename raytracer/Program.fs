// https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays

open SixLabors.ImageSharp
open System.IO
open System
open System.Numerics
open Numerics
open Numerics
open Numerics

let width = 640
let height = 480

let raySampes = [0.1f..0.1f..100.0f]
let frameBuffer = Array2D.create width height Rgba32.DarkGray
let squareRange = [200..300]

let render = 
    using (File.OpenWrite("test.jpg")) (fun output ->
        using(new Image<Rgba32>(width,height))(fun image -> 
            for px in 0..width-1 do
                for py in 0..height-1 do
                    if List.exists ( px.Equals ) squareRange &&  List.exists (py.Equals) squareRange then
                        image.Item(px,py) <- Rgba32.White        
                    else
                        image.Item(px,py) <- frameBuffer.[px,py]
                    
            image.SaveAsJpeg(output)
        )
    )  


let testCameraInv =
    let pos = Vector3(2.0f,2.5f,-10.0f)
    let target =  Vector3(3.0f,0.7f,4.0f)
    let up = Vector3.UnitY
    let worldToCamera = Numerics.worldToCamera pos  target up
    let inv_1 = Numerics.cameraToWorld worldToCamera
    let inv_2 = Numerics.cameraToWorldFast worldToCamera 6
    let I_1 = worldToCamera*inv_1
    let I_2 = inv_2*worldToCamera 
    let buff = 1 // hack to display I_2 in VS Code editor while debugging
    ()

[<EntryPoint>]
let main argv =
    render
    0 // return an integer exit code





    
