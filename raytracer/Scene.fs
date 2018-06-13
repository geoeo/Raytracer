// https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays
module Raytracer.Scene

open SixLabors.ImageSharp
open System.IO
open System
open System.Numerics
open Raytracer.Camera
open Raytracer.Surface
open Raytracer.Geometry
open Raytracer.Material
open Raytracer.Numerics
open Raytracer.SceneDefinitions
open Henzai.Sampling
open BenchmarkDotNet.Attributes


type Scene () =

    let width = 400
    let height = 400
    let samples = 10000
    let maxTraceDepth = 10us
    let backgroundColor = Vector3.Zero
    // let frameBuffer = Array2D.create width height defaultColor
    let frameBuffer = Array2D.create width height Vector4.Zero
    let depthBuffer = Array2D.create width height System.Single.MaxValue
    let mutable maxFrameBufferDepth = 0.0f


    let randomState = new Random()
        // let lightWS = Vector3(4.0f, 3.0f, -5.0f)

    //let planes = []
    let surfaces : (Surface array) = scene_spheres |> Array.ofList

    let cameraOriginWS = Vector3(-3.0f,6.0f,15.0f)
    let lookAt = Vector3(-1.0f,1.0f,-10.0f)

    let viewMatrix = WorldToCamera cameraOriginWS lookAt Vector3.UnitY

    let cameraWS = CameraToWorld viewMatrix 

    let fov = MathF.PI/4.0f

    let writeToDepthBuffer t px py = 
        depthBuffer.[px,py] <- t
        if t > maxFrameBufferDepth then 
            maxFrameBufferDepth <- t  

    let weighting x = MathF.Pow(0.5f,x)

    let weightList (contributions : Raytracer.Material.Color list) =
        // let stepSize = MathF.PI/(2.0f*((float32)contributions.Length))
        let stepSize = MathF.PI/(2.0f*((float32)contributions.Length))
        // let weightingSamples = List.map MathF.Cos [0.0f..stepSize..(MathF.PI/2.0f-stepSize)]
        let weightingSamples = List.map weighting [0.0f..1.0f..((float32)contributions.Length-1.0f)]
        List.map (fun (a,b) -> a*b) (List.zip weightingSamples contributions)

    let CalcNormFactor (input :Vector3) = (Vector3.Normalize(input/MathF.Max(input.X,MathF.Max(input.Y,input.Z))))

    //Todo: investigage attenuation + color spaces
    //TODO: check how rays are composed. If the last ray cotributes, all rays should
    let composeContributions (contributions : Raytracer.Material.Color list) = 
        match contributions with 
            | [] -> Vector3.Zero
            | list -> List.reduce (fun acc shading -> acc*shading)  ((list))


    //TODO: check how rays are composed. If the last ray cotributes, all rays should
    let updateContributions doesRayContribute (emitted:Color) (scattered:Color) contributions =
        match  doesRayContribute with   
            | true -> ((emitted + scattered) :: contributions) 
            | false -> (emitted :: contributions)

    let rec rayTrace (ray : Ray) previousTraceDepth (accEmitted : Color) (accScatter :Color) =
        if previousTraceDepth > maxTraceDepth 
        then  
            accEmitted + backgroundColor*accScatter
        else
            let newTraceDepth = previousTraceDepth + 1us
            let (realSolution,t,surface) = findClosestIntersection ray surfaces
            let surfaceGeometry : Hitable = surface.Geometry
            if surfaceGeometry.IntersectionAcceptable realSolution t 1.0f (PointForRay ray t)
            then
                let (doesRayContribute,outRay,scatteredShading) = surface.Scatter ray t ((int)newTraceDepth)
                let emittedShading = surface.Emitted
                let e = accEmitted + accScatter*emittedShading 
                let s = accScatter*scatteredShading
                match doesRayContribute with
                    | true -> (rayTrace outRay newTraceDepth e s)
                    | false -> emittedShading
            else 
               accEmitted + backgroundColor*accScatter

    let rayTraceBase (ray : Ray) px py writeToDepth = 
        let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
        // let allOtherSurfaces = (AllSurfacesWithoutId surfaces ray.SurfaceOrigin)
        let (realSolution,t,surface) = findClosestIntersection ray surfaces
        let surfaceGeometry = surface.Geometry
        if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay (PointForRay ray t) then
            let (doesRayContribute,outRay,scatteredShading) = surface.Scatter ray t 0
            if writeToDepth then writeToDepthBuffer t px py
            let emittedShading = surface.Emitted
            match doesRayContribute with
                | true -> emittedShading + scatteredShading*(rayTrace outRay 1us Vector3.Zero Vector3.One)
                | false -> emittedShading
        else
            backgroundColor

    let pertrube px py = 
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt
        let randVec = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
        let xOff = randVec.X/((float32)(width))
        let yOff = randVec.Y/((float32)(height))
        // let xOff = randVec.X/2.0f
        // let yOff = randVec.Y/2.0f
        ((float32)px + xOff, (float32)py+yOff)
        
    let renderPass px py = 
        let (pxOffset,pyOffset) = pertrube px py
        let dirCS = 
            RayDirection (PixelToCamera pxOffset pyOffset (float32 width) (float32 height) fov)
        let rot = Rotation cameraWS
        let dirWS = Vector3.Normalize(Vector3.TransformNormal(dirCS,rot))
        let ray = Ray(cameraWS.Translation, dirWS)
        let color = rayTraceBase ray px py true
        let colorSamples = [|for _ in 0..(samples-1) -> async {return rayTraceBase ray px py false}|]
        let colors =  colorSamples |> Async.Parallel |> Async.RunSynchronously
        let avgColor = (Array.reduce (fun acc c -> acc+c) colors + color)/(float32)samples
        //printfn "Completed Ray for pixels (%i,%i)" px py
        //async {printfn "Completed Ray for pixels (%i,%i)" px py} |> Async.StartAsTask |> ignore
        //Gamma correct TODO: refactor
        frameBuffer.[px,py] <- Vector4(Vector3.SquareRoot(avgColor),1.0f)


    [<Benchmark>]
    member self.renderScene () =
        for px in 0..width-1 do
            for py in 0..height-1 do
                renderPass px py

    member self.saveFrameBuffer () =
        using (File.OpenWrite("sphere.jpg")) (fun output ->
            using(new Image<Rgba32>(width,height))(fun image -> 
                for px in 0..width-1 do
                    for py in 0..height-1 do
                        image.Item(px,py) <- Rgba32(frameBuffer.[px,py])
                    
                image.SaveAsJpeg(output)
            )
        )

    member self.saveDepthBuffer () =
        using (File.OpenWrite("depth.jpg")) (fun output ->
            using(new Image<Rgba32>(width,height))(fun image -> 
                for px in 0..width-1 do
                    for py in 0..height-1 do
                        let color = Vector4(Vector3(depthBuffer.[px,py]/maxFrameBufferDepth),1.0f)
                        image.Item(px,py) <- Rgba32(color)
                    
                image.SaveAsJpeg(output)
            )
        )