// https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays

module Raytracer.Scene

open SixLabors.ImageSharp
open System.IO
open System
open System.Numerics
open Raytracer.Surface
open Raytracer.Camera
open Raytracer.Geometry
open Raytracer.Material

let width = 640
let height = 480

let raySampes = [0.1f..0.1f..100.0f]

let defaultColor = Rgba32.DarkGray

// let frameBuffer = Array2D.create width height defaultColor
let frameBuffer = Array2D.create width height Vector4.One
let depthBuffer = Array2D.create width height System.Single.MaxValue
let mutable maxFrameBufferDepth = 0.0f
let squareRange = [200..300]

let maxRecursionLevel = 1us

let mutable id : ID = 1UL

let assignIDAndIncrement idIn : ID =
    let toBeAssigned = idIn
    id <- id + 1UL
    toBeAssigned

let spheres 
    = [
        Lambertian(assignIDAndIncrement id,Sphere(Vector3(0.0f,0.0f,-10.0f),2.0f), Material(Vector3(0.0f,1.0f,0.0f)));
        Lambertian(assignIDAndIncrement id,Sphere(Vector3(-5.0f,0.0f,-20.0f),5.0f),Material(Vector3(1.0f,0.0f,0.0f)))
      ]
// let spheres = []


let planes = [Lambertian(assignIDAndIncrement id,Plane(Plane.CreateFromVertices(Vector3(-1.0f,-6.0f,0.0f),Vector3(1.0f,-6.0f,0.0f),Vector3(0.0f,-6.0f,-1.0f))),Material(Vector3(1.0f,1.0f,1.0f)))]
//let planes = []
let surfaces : (Surface list) = List.concat [List.map Surface.ToSurface spheres;List.map Surface.ToSurface planes]

let cameraOriginWS = Vector3(-1.0f,6.0f,10.0f)
let lookAt = Vector3(0.0f,1.0f,-10.0f)

let lightWS = Vector3(4.0f, 3.0f, -5.0f)
// let lightWS = Vector3(-1.0f, 6.0f, 10.0f)
let viewMatrix = worldToCamera cameraOriginWS lookAt Vector3.UnitY

let cameraWS = cameraToWorld viewMatrix 

let fov = MathF.PI/4.0f

let weightList (contributions : Raytracer.Material.Color list) =
    let stepSize = MathF.PI/(2.0f*((float32)contributions.Length))
    let weightingSamples = List.map MathF.Cos [0.0f..stepSize..(MathF.PI/2.0f-stepSize)]
    List.map (fun (a,b) -> a*b) (List.zip weightingSamples contributions)




//Todo: investigage attenuation + color spaces
//TODO: check how rays are composed. If the last ray cotributes, all rays should
let composeContributions (contributions : Raytracer.Material.Color list) = 
    match contributions with 
        | [] -> Vector3.Zero
        | list -> List.reduce (fun acc shading -> acc+shading)  (weightList (List.rev contributions))


let updateContributions doesRayContribute shading contributions =
    match  doesRayContribute with   
        | true -> (shading :: contributions) 
        | false -> contributions

let rec rayTrace (ray : Ray) (contributions : Raytracer.Material.Color list) traceLevel =
    if traceLevel >= maxRecursionLevel 
    then 
        contributions
    else
        let newLevel = traceLevel + 1us
        let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
        let (realSolution,t,surface) = findClosestIntersection ray (AllSurfacesWithoutId surfaces ray.SurfaceOrigin)
        let surfaceGeometry = surface.Geometry
        if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay 
        then
            let positionOnSurface = ray.Origin + t*ray.Direction
            let allOtherGeometries = SurfacesToGeometry (AllSurfacesWithoutId surfaces surface.ID)
            let (doesRayContribute,outRay,shading) = surface.Scatter ray positionOnSurface lightWS allOtherGeometries 
            (rayTrace outRay (updateContributions doesRayContribute shading contributions) newLevel)
        else 
            contributions

let rayTraceBase (ray : Ray) px py = 
    let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
    let (realSolution,t,surface) = findClosestIntersection ray surfaces
    let surfaceGeometry = surface.Geometry
    if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay then
        let positionOnSurface = ray.Origin + t*ray.Direction
        let allOtherGeometries = SurfacesToGeometry (AllSurfacesWithoutId surfaces surface.ID)
        let (doesRayContribute,outRay,shading) = surface.Scatter ray positionOnSurface lightWS allOtherGeometries
        if t < depthBuffer.[px,py] then
            let color = composeContributions(updateContributions doesRayContribute shading (rayTrace outRay [] 0us))
            frameBuffer.[px,py] <- Vector4(color,1.0f)
            depthBuffer.[px,py] <- t
            if t > maxFrameBufferDepth then 
                maxFrameBufferDepth <- t  



let renderScene = lazy
    for px in 0..width-1 do
        for py in 0..height-1 do
            let dirCS = 
                rayDirection (pixelToCamera (float32 px) (float32 py) (float32 width) (float32 height) fov)
            let rot = rotation cameraWS
            let dirWS = Vector3.Normalize(Vector3.TransformNormal(dirCS,rot))
            let ray = Ray(cameraWS.Translation, dirWS)
            rayTraceBase ray px py |> ignore 




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
                    let color = Vector4(Vector3(depthBuffer.[px,py]/maxFrameBufferDepth),1.0f)
                    image.Item(px,py) <- Rgba32(color)
                
            image.SaveAsJpeg(output)
        )
    )