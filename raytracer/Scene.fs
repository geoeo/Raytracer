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

//TODO:Use Refactor this
let inline setUpMonteCarlo (surface : Surface) (ray : Ray) (t : LineParameter) (accEmitted : Color) (accScatter :Color) (currentTraceDepth : uint32) =

        let emittedShading = surface.Emitted
        let e = accEmitted + accScatter*emittedShading 
        let mcSamples = surface.SampleCount
        //let eMCAdjusted = e / surface.MCNormalization

        let rayTraceParameters = 
            [|
                for _ in 1..mcSamples do
                    let (doesRayContribute,outRay,cosOfIncidence) = surface.Scatter ray t ((int)currentTraceDepth)
                    let shading = surface.BRDF*cosOfIncidence / (surface.PDF*surface.MCNormalization)
                    let s = accScatter*shading
                    yield (currentTraceDepth, outRay , e , s)
            |]
        rayTraceParameters 

 
type Scene () =

    let width = 800
    let height = 640
    let samples = 4
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


    let rec rayTrace previousTraceDepth ((ray : Ray) , (accEmitted : Color) , (accScatter :Color)) =
        if previousTraceDepth > maxTraceDepth 
        then  
            accEmitted + backgroundColor*accScatter
        else
            let newTraceDepth = previousTraceDepth + 1us
            let (realSolution,t,surface) = findClosestIntersection ray surfaces
            let surfaceGeometry : Hitable = surface.Geometry
            if surfaceGeometry.IntersectionAcceptable realSolution t 1.0f (PointForRay ray t)
            then
                let emittedShading = surface.Emitted
                let e = accEmitted + accScatter*emittedShading 
                let mcSamples = surface.SampleCount
                
                //TODO rewrite this to make it tail recursive
                let mutable totalShading = e/surface.MCNormalization
                for _ in 1..mcSamples do
                    let (doesRayContribute,outRay,cosOfIncidence) = surface.Scatter ray t ((int)newTraceDepth)
                    let shading = surface.BRDF*cosOfIncidence / (surface.PDF*surface.MCNormalization)
                    let s = accScatter*shading
                    totalShading <- totalShading + (rayTrace newTraceDepth (outRay , e , s))
                totalShading
                

            else 
                accEmitted + backgroundColor*accScatter


    let rayTraceBase (ray : Ray) px py iteration = 
        let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
        let (realSolution,t,surface) = findClosestIntersection ray surfaces
        let surfaceGeometry = surface.Geometry
        if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay (PointForRay ray t) then
            if iteration = 1 then writeToDepthBuffer t px py

            let emittedShading = surface.Emitted
            let mcSamples = surface.SampleCount
            let mutable totalShading = emittedShading/surface.MCNormalization
            for _ in 0..mcSamples-1 do
                let (doesRayContribute,outRay,cosOfIncidence) = surface.Scatter ray t 0
                let shading = surface.BRDF*cosOfIncidence / (surface.PDF*surface.MCNormalization)
                totalShading <- totalShading + (rayTrace 1us (outRay , emittedShading , shading)  )
            totalShading
            
        else
            backgroundColor 


    let rec rayTraceAsync (previousTraceDepth , (ray : Ray) , (accEmitted : Color) , (accScatter :Color)) =
        async {
            if previousTraceDepth > maxTraceDepth 
            then  
                return accEmitted + backgroundColor*accScatter
            else
                let newTraceDepth = previousTraceDepth + 1us
                let (realSolution,t,surface) = findClosestIntersection ray surfaces
                let surfaceGeometry : Hitable = surface.Geometry
                if surfaceGeometry.IntersectionAcceptable realSolution t 1.0f (PointForRay ray t)
                then
                    let emittedShading = surface.Emitted
                    let e = accEmitted + accScatter*emittedShading 
                    let mcSamples = surface.SampleCount

                    let rayTraceParameters = 
                        [|
                            for _ in 1..mcSamples do
                                let (doesRayContribute,outRay,cosOfIncidence) = surface.Scatter ray t ((int)newTraceDepth)
                                let shading = surface.BRDF*cosOfIncidence / (surface.PDF*surface.MCNormalization)
                                let s = accScatter*shading
                                yield (newTraceDepth, outRay , e , s)
                        |]   
                    if Array.isEmpty rayTraceParameters then
                        return e
                    else 
                        let eMCAdjusted = e / surface.MCNormalization
                        let rayTracesParallel = rayTraceParameters |> Array.map rayTraceAsync |> Async.Parallel   
                        let! rayTraces = rayTracesParallel 
                        return Array.sumBy (fun x -> eMCAdjusted + x) rayTraces

                else 
                    return accEmitted + backgroundColor*accScatter
        }
            
    let rayTraceBaseAsync (ray : Ray) px py iteration = 
        async {
            let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
            let (realSolution,t,surface) = findClosestIntersection ray surfaces
            let surfaceGeometry = surface.Geometry
            if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay (PointForRay ray t) then
                if iteration = 1 then writeToDepthBuffer t px py

                let emittedShading = surface.Emitted
                let mcSamples = surface.SampleCount
                let rayTraceParameters = 
                    [|
                        for _ in 1..mcSamples do
                            let (doesRayContribute,outRay,cosOfIncidence) = surface.Scatter ray t 1
                            let shading = surface.BRDF*cosOfIncidence / (surface.PDF*surface.MCNormalization)
                            let s = shading
                            yield (1us, outRay , emittedShading , s)
                    |]   
                if Array.isEmpty rayTraceParameters then
                    return emittedShading
                else 
                    let eMCAdjusted = emittedShading / surface.MCNormalization
                    let rayTracesParallel = rayTraceParameters |> Array.map rayTraceAsync |> Async.Parallel   
                    let! rayTraces = rayTracesParallel 
                    return Array.sumBy (fun x -> eMCAdjusted + x) rayTraces
                
            else
                return backgroundColor

        }

    let renderPass px py = 
        let dirCS = 
            RayDirection (PixelToCamera (float32 px) (float32 py) (float32 width) (float32 height) fov)
        let rot = Rotation cameraWS
        let dirWS = Vector3.Normalize(Vector3.TransformNormal(dirCS,rot))
        let ray = Ray(cameraWS.Translation, dirWS)
        let colorSamples = [|for i in 1..samples -> rayTraceBaseAsync ray px py i|]
        let colors =  colorSamples |> Async.Parallel |> Async.RunSynchronously
        let avgColor = if Array.isEmpty colorSamples then Vector3.Zero else (Array.reduce (fun acc c -> acc+c) colors)/(float32)samples
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