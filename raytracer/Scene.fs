// https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays
module Raytracer.Scene

open SixLabors.ImageSharp
open System.IO
open System
open System.Numerics
open Raytracer.Camera
open Raytracer.Geometry
open Raytracer.Material
open Raytracer.Numerics
open Raytracer.Surface
open Raytracer.SceneDefinitions
open BenchmarkDotNet.Attributes

type Scene () =

    let width = 800
    let height = 640
    let samplesPerPixel = 4
    let batchSize = 4
    let batches = samplesPerPixel / batchSize
    let batchIndices = [|1..batchSize|]
    let colorSamples = Array.create samplesPerPixel Vector3.Zero
    let colorSamplesClear = Array.create samplesPerPixel Vector3.Zero

    // let frameBuffer = Array2D.create width height defaultColor
    let frameBuffer = Array2D.create width height Vector4.Zero
    let depthBuffer = Array2D.create width height System.Single.MaxValue
    let mutable maxFrameBufferDepth = 0.0f
    let maxTraceDepth = 5us
    let backgroundColor = Vector3.Zero
    let surfaces : (Surface array) = scene_spheres |> Array.ofList

        // let lightWS = Vector3(4.0f, 3.0f, -5.0f)

    let cameraOriginWS = Vector3(-3.0f,6.0f,15.0f)
    let lookAt = Vector3(-1.0f,1.0f,-10.0f)

    let viewMatrix = WorldToCamera cameraOriginWS lookAt Vector3.UnitY

    let cameraWS = CameraToWorld viewMatrix 

    let fov = MathF.PI/4.0f

    let writeToDepthBuffer t px py = 
        depthBuffer.[px,py] <- t
        if t > maxFrameBufferDepth then 
            maxFrameBufferDepth <- t 


    let rec rayTrace (previousTraceDepth , (ray : Ray) , (accEmitted : Color) , (accScatter :Color)) =
        if previousTraceDepth > maxTraceDepth 
        then  
            accEmitted + backgroundColor*accScatter
        else
            let currentTraceDepth = previousTraceDepth + 1us
            let (realSolution,t,surface) = findClosestIntersection ray surfaces
            let surfaceGeometry : Hitable = surface.Geometry
            if surfaceGeometry.IntersectionAcceptable realSolution t 1.0f (PointForRay ray t)
            then
                let emittedRadiance = surface.Emitted
                let e = accEmitted + accScatter*emittedRadiance 
                let (validSamples,raySamples) = surface.GenerateSamples ray t ((int)currentTraceDepth) surface.SamplesArray
                if validSamples = 0 then
                    e
                else 
                    //let eMCAdjusted = e / surface.MCNormalization
                    //let rayTraces = raySamples  |>  Array.map ((fun (o ,s) -> (currentTraceDepth,o,e,accScatter*s) ) >> rayTrace) 
                    //Array.sumBy (fun x -> eMCAdjusted + x) rayTraces
                    let mutable totalLight = emittedRadiance / (float32)validSamples
                    for i in 0..validSamples-1 do
                        let (ray,shading) = raySamples.[i]
                        totalLight <- totalLight + rayTrace (currentTraceDepth,ray,emittedRadiance,accScatter*shading)
                    totalLight
                
            else 
                accEmitted + backgroundColor*accScatter


    let rayTraceBase (ray : Ray) px py iteration batchIndex = 
        let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
        let (realSolution,t,surface) = findClosestIntersection ray surfaces
        let surfaceGeometry = surface.Geometry
        if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay (PointForRay ray t) then
            if iteration = 1 && batchIndex = 0 then writeToDepthBuffer t px py

            let currentTraceDepth = 0us
            let emittedRadiance = surface.Emitted
            let (validSamples,raySamples) = surface.GenerateSamples ray t ((int)currentTraceDepth) surface.SamplesArray
            if validSamples = 0 then
                emittedRadiance
            else 
                //let eMCAdjusted = emittedRadiance / surface.MCNormalization
                //let rayTraces = raySamples  |>  Array.map ((fun (o ,s) -> (currentTraceDepth,o,emittedRadiance,s) ) >> rayTrace) 
                //Array.sumBy (fun x -> eMCAdjusted + x) rayTraces
                let mutable totalLight = emittedRadiance / (float32)validSamples
                for i in 0..validSamples-1 do
                    let (ray,shading) = raySamples.[i]
                    totalLight <- totalLight + rayTrace (currentTraceDepth,ray,emittedRadiance,shading)
                totalLight
            
        else
            backgroundColor 

    let renderPass px py = 
        let dirCS = 
            RayDirection (PixelToCamera (float32 px) (float32 py) (float32 width) (float32 height) fov)
        let rot = Rotation cameraWS
        let dirWS = Vector3.Normalize(Vector3.TransformNormal(dirCS,rot))
        let ray = Ray(cameraWS.Translation, dirWS)
        // V1 - Slow (A lot of GC because of async blocks!)
        //let colorSamples = [|for i in 1..samples -> rayTraceBaseAsync ray px py i|]
        //let colors =  colorSamples |> Async. Parallel |> Async.RunSynchronously
        //let avgColor = if Array.isEmpty colorSamples then Vector3.Zero else (Array.reduce (fun acc c -> acc+c) colors)/(float32)samples
        //V2 - Fastest
        //TODO: Prealocate for faster runtime
        //let colorSamples = Array.create samplesPerPixel Vector3.Zero
        for batchIndex in 0..batches-1 do
            let colorSamplesBatch = Array.map (fun i -> async {return rayTraceBase ray px py i batchIndex}) batchIndices
            let colorsBatch =  colorSamplesBatch |> Async.Parallel |> Async.RunSynchronously
            //let colorsBatch = Array.Parallel.map (fun i -> rayTraceBase ray px py i batchIndex) batchIndices
            Array.blit colorsBatch 0 colorSamples (batchIndex*batchSize) batchSize 
        let avgColor = if Array.isEmpty colorSamples then Vector3.Zero else (Array.reduce (fun acc c -> acc+c) colorSamples)/(float32)samplesPerPixel
        Array.blit colorSamplesClear 0 colorSamples 0 samplesPerPixel 
        //printfn "Completed Ray for pixels (%i,%i)" px py
        //async {printfn "Completed Ray for pixels (%i,%i)" px py} |> Async.StartAsTask |> ignore
        //Gamma correct TODO: refactor
        frameBuffer.[px,py] <- Vector4(Vector3.SquareRoot(avgColor),1.0f)
        // frameBuffer.[px,py] <- Vector4(avgColor,1.0f)

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