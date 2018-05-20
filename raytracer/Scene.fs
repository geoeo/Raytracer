// https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays

module Raytracer.Scene

open SixLabors.ImageSharp
open System.IO
open System
open System.Numerics
open Raytracer.Numerics
open Raytracer.Camera
open Raytracer.Geometry

let width = 640
let height = 480

let raySampes = [0.1f..0.1f..100.0f]

let defaultColor = Rgba32.DarkGray

// let frameBuffer = Array2D.create width height defaultColor
let frameBuffer = Array2D.create width height Vector4.One
let depthBuffer = Array2D.create width height System.Single.MaxValue
let mutable maxDepth = 0.0f
let squareRange = [200..300]

let mutable id : ID = (uint64)1

let assignIDAndIncrement idIn : ID =
    let toBeAssigned = idIn
    id <- id + (uint64)1
    toBeAssigned

let spheres = [Sphere(Vector3(0.0f,0.0f,-10.0f),2.0f,assignIDAndIncrement id);Sphere(Vector3(-5.0f,0.0f,-20.0f),5.0f,assignIDAndIncrement id)]
//let spheres = []

let planes = [Plane(Plane.CreateFromVertices(Vector3(-1.0f,-6.0f,0.0f),Vector3(1.0f,-6.0f,0.0f),Vector3(0.0f,-6.0f,-1.0f)),assignIDAndIncrement id)]

let surfaces : (Hitable list) = List.concat [List.map (fun x -> x:> Hitable) spheres; List.map (fun x -> x:> Hitable) planes]

let cameraOriginWS = Vector3(0.0f,2.0f,0.0f)
let target = Vector3(0.0f,0.0f,-10.0f)

let lightWS = Vector3(0.0f, 10.0f, -5.0f)
let viewMatrix = worldToCamera cameraOriginWS target Vector3.UnitY

let cameraWS = cameraToWorld viewMatrix 

let fov = MathF.PI/4.0f

let render = lazy
    using (File.OpenWrite("test.jpg")) (fun output ->
        using(new Image<Rgba32>(width,height))(fun image -> 
            for px in 0..width-1 do
                for py in 0..height-1 do
                    if List.exists ( px.Equals ) squareRange &&  List.exists (py.Equals) squareRange then
                        image.Item(px,py) <- Rgba32.White        
                    else
                        image.Item(px,py) <- defaultColor
                    
            image.SaveAsJpeg(output)
        )
    )  

let renderScene = lazy
    for px in 0..width-1 do
        for py in 0..height-1 do
            let dirCS = 
                rayDirection (pixelToCamera (float32 px) (float32 py) (float32 width) (float32 height) fov)
            let rot = rotation cameraWS
            let dirWS = Vector3.TransformNormal(dirCS,rot)
            let dirNormalized = Vector3.Normalize(dirWS)
            let ray = Ray(cameraWS.Translation, dirNormalized)
            let dotViewAndTracingRay = Vector3.Dot(Vector3.Normalize(target),dirNormalized)
            for sphere in spheres do 
                let (realSolution,t) = sphere.Intersect ray
                if sphere.IntersectionAcceptable realSolution t dotViewAndTracingRay then
                    let positionOnSphere = cameraOriginWS + t*dirNormalized
                    let normal = sphere.NormalForSurfacePoint positionOnSphere
                    let pointToLight = Vector3.Normalize(lightWS - positionOnSphere)
                    let diffuse = Vector3.Dot(normal,pointToLight)
                    if t < depthBuffer.[px,py] then
                        let color = Vector4(1.0f,1.0f,1.0f,1.0f)*diffuse
                        frameBuffer.[px,py] <- color
                        depthBuffer.[px,py] <- t
                        if t > maxDepth then 
                            maxDepth <- t
            for plane in planes do 
                let (realSolution,t) = plane.Intersect ray
                if plane.IntersectionAcceptable realSolution t dotViewAndTracingRay then
                    let positionOnPlane = cameraOriginWS + t*dirNormalized
                    let normal = plane.NormalForSurfacePoint positionOnPlane
                    let pointToLight = Vector3.Normalize(lightWS - positionOnPlane)
                    // TODO refactor this into generic recusive algorithm!
                    let rayHitToLight = Ray(positionOnPlane,pointToLight)
                    let diffuse = Vector3.Dot(normal,pointToLight)*lightTransportWithObstacle(isRayObstructed spheres rayHitToLight)
                    if t < depthBuffer.[px,py] then
                        let color = Vector4(0.0f,0.0f,1.0f,1.0f)*diffuse
                        frameBuffer.[px,py] <- color
                        depthBuffer.[px,py] <- t
                        if t > maxDepth then 
                            maxDepth <- t
        



let renderSurfaces = lazy
    for px in 0..width-1 do
        for py in 0..height-1 do
            let dirCS = 
                rayDirection (pixelToCamera (float32 px) (float32 py) (float32 width) (float32 height) fov)
            let rot = rotation cameraWS
            let dirWS = Vector3.TransformNormal(dirCS,rot)
            let dirNormalized = Vector3.Normalize(dirWS)
            let ray = Ray(cameraWS.Translation, dirNormalized)
            let dotViewAndTracingRay = Vector3.Dot(Vector3.Normalize(target),dirNormalized)
            for surface in surfaces do 
                let (realSolution,t) = surface.Intersect ray
                if surface.IntersectionAcceptable realSolution t dotViewAndTracingRay then
                    let positionOnSurface = cameraOriginWS + t*dirNormalized
                    let normal = surface.NormalForSurfacePoint positionOnSurface
                    let pointToLight = Vector3.Normalize(lightWS - positionOnSurface)
                    // TODO refactor this into generic recusive algorithm!
                    let rayHitToLight = Ray(positionOnSurface,pointToLight)
                    // TODO refator insection to take all objects except current
                    let diffuse = Vector3.Dot(normal,pointToLight)*lightTransportWithObstacle(surface.IsRayObstructed (List.map Hitable.ToHitable spheres) rayHitToLight)
                    if t < depthBuffer.[px,py] then
                        let color = surface.Color*diffuse
                        frameBuffer.[px,py] <- color
                        depthBuffer.[px,py] <- t
                        if t > maxDepth then 
                            maxDepth <- t   


let saveFrameBuffer = lazy
    using (File.OpenWrite("sphere.jpg")) (fun output ->
        using(new Image<Rgba32>(width,height))(fun image -> 
            for px in 0..width-1 do
                for py in 0..height-1 do
                    image.Item(px,py) <- Rgba32(frameBuffer.[px,py])
                
            image.SaveAsJpeg(output)
        )
    )

let saveDepthBuffer = lazy
    using (File.OpenWrite("depth.jpg")) (fun output ->
        using(new Image<Rgba32>(width,height))(fun image -> 
            for px in 0..width-1 do
                for py in 0..height-1 do
                    let color = Vector4(Vector3(depthBuffer.[px,py]/maxDepth),1.0f)
                    image.Item(px,py) <- Rgba32(color)
                
            image.SaveAsJpeg(output)
        )
    )


let testCameraInv = lazy
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