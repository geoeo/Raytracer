﻿// https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays

open SixLabors.ImageSharp
open System.IO
open System
open System.Numerics
open Raytracer.Numerics
open Camera
open Geometry

let width = 640
let height = 480

let raySampes = [0.1f..0.1f..100.0f]
let frameBuffer = Array2D.create width height Rgba32.DarkGray
let squareRange = [200..300]

let shapes = [(Vector3(0.0f,0.0f,-10.0f),2.0f)]
// let cameraOriginWS = Vector3(0.0f,0.0f,0.0f)
// let target = -Vector3.UnitZ
let cameraOriginWS = Vector3(0.0f,5.0f,0.0f)
let target = Vector3(0.0f,0.0f,-10.0f)

let lightWS = Vector3(0.0f, 20.0f, 0.0f)
let viewMatrix = worldToCamera cameraOriginWS target Vector3.UnitY

let cameraWS = cameraToWorld viewMatrix 

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

let render_sphere = 
    using (File.OpenWrite("sphere.jpg")) (fun output ->
        using(new Image<Rgba32>(width,height))(fun image -> 
            for px in 0..width-1 do
                for py in 0..height-1 do
                    let dir = 
                        rayDirection (pixelToCamera (float32 px) (float32 py) (float32 width) (float32 height) (MathF.PI/4.0f))
                    let rot = rotation cameraWS
                    let dirWS = Vector3.TransformNormal(dir,rot)
                    let dirNormalized = Vector3.Normalize(dirWS)
                    let ray = Ray(cameraWS.Translation, dirNormalized)
                    for (origin,radius) in shapes do 
                        let (realSolution,i1,i2) = sphereIntersections (origin,radius)  ray
                        let closestInterection = smallestNonNegative (i1,i2)
                        let positionOnSphere = cameraOriginWS + closestInterection*dirNormalized
                        let normal = sphereNormal positionOnSphere origin
                        let pointToLight = Vector3.Normalize(lightWS - origin)
                        let diffuse = Vector3.Dot(normal,pointToLight)
                        if realSolution then
                            let color = Vector4(1.0f,1.0f,1.0f,1.0f)*diffuse
                            image.Item(px,py) <- Rgba32(color)   
                        else
                            image.Item(px,py) <- frameBuffer.[px,py]

                    
            image.SaveAsJpeg(output)
        )
    )  


let testCameraInv =
    let pos = Vector3(2.0f,2.5f,-10.0f)
    let target =  Vector3(3.0f,0.7f,4.0f)
    let up = Vector3.UnitY
    let worldToCamera = worldToCamera pos  target up
    let inv_1 = cameraToWorld worldToCamera
    let inv_2 = cameraToWorldFast worldToCamera 6
    let I_1 = worldToCamera*inv_1
    let I_2 = inv_2*worldToCamera 
    let buff = 1 // hack to display I_2 in VS Code editor while debugging
    ()

[<EntryPoint>]
let main argv =
    render_sphere
    0 // return an integer exit code





    
