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
open Henzai.Sampling
open BenchmarkDotNet.Attributes

type Scene () =

    let width = 640
    let height = 480
    let samples = 100

    let backgroundColor = Vector3.Zero

    // let frameBuffer = Array2D.create width height defaultColor
    let frameBuffer = Array2D.create width height Vector4.Zero
    let depthBuffer = Array2D.create width height System.Single.MaxValue
    let mutable maxFrameBufferDepth = 0.0f

    let maxTraceDepth = 5us

    let mutable id : ID = 1UL

    let randomState = new Random()

    let assignIDAndIncrement idIn : ID =
        let toBeAssigned = idIn
        id <- id + 1UL
        toBeAssigned

        // let lightWS = Vector3(4.0f, 3.0f, -5.0f)

    let lights : Surface list 
        =  [
            Emitting(assignIDAndIncrement id, Sphere(Vector3(3.0f, 8.0f, -5.0f),3.0f), Material(Vector3(1.0f,1.0f,1.0f)))
            Emitting(assignIDAndIncrement id, Sphere(Vector3(-4.0f, 8.0f, -10.0f),3.0f), Material(Vector3(1.0f,1.0f,1.0f)))
        ]

    let spheres : Surface list
        = [
            Lambertian(assignIDAndIncrement id,Sphere(Vector3(2.0f,0.0f,-14.0f),2.0f), Material(Vector3(0.0f,1.0f,0.0f)));
            // Lambertian(assignIDAndIncrement id,Sphere(Vector3(-1.5f,0.0f,-14.0f),2.0f),Material(Vector3(0.0f,0.0f,1.0f)))
            Metal(assignIDAndIncrement id,Sphere(Vector3(-5.0f,0.0f,-20.0f),5.0f),Material(Vector3(0.0f,0.0f,1.0f)),0.5f)
          ]
    // let spheres = []

    let planes : Surface list = [Lambertian(assignIDAndIncrement id,Plane(Plane.CreateFromVertices(Vector3(-1.0f,-6.0f,0.0f),Vector3(1.0f,-6.0f,0.0f),Vector3(0.0f,-6.0f,-1.0f))),Material(Vector3(1.0f,1.0f,1.0f)))]
    //let planes = []
    let surfaces : (Surface list) = List.concat [spheres;planes;lights]

    let cameraOriginWS = Vector3(-1.0f,6.0f,10.0f)
    let lookAt = Vector3(0.0f,1.0f,-10.0f)
    let viewMatrix = worldToCamera cameraOriginWS lookAt Vector3.UnitY

    let cameraWS = cameraToWorld viewMatrix 

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

    let rec rayTrace (ray : Ray) previousTraceDepth =
        if previousTraceDepth > maxTraceDepth 
        then  
            backgroundColor
        else
            let newTraceDepth = previousTraceDepth + 1us
            let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(-Vector3.UnitZ),ray.Direction)
            let (realSolution,t,surface) = findClosestIntersection ray (AllSurfacesWithoutId surfaces ray.SurfaceOrigin)
            let surfaceGeometry : Hitable = surface.Geometry
            if surfaceGeometry.IntersectionAcceptable realSolution t 1.0f 
            then
                let (doesRayContribute,outRay,scatteredShading) = surface.Scatter ray t ((int)newTraceDepth)
                let emittedShading = surface.Emitted
                match doesRayContribute with
                    | true -> emittedShading + scatteredShading*(rayTrace outRay newTraceDepth)
                    | false -> emittedShading
            else 
               backgroundColor

    let rayTraceBase (ray : Ray) px py writeToDepth = 
        let dotLookAtAndTracingRay = Vector3.Dot(Vector3.Normalize(lookAt),ray.Direction)
        let allOtherSurfaces = (AllSurfacesWithoutId surfaces ray.SurfaceOrigin)
        let (realSolution,t,surface) = findClosestIntersection ray allOtherSurfaces
        let surfaceGeometry = surface.Geometry
        if surfaceGeometry.IntersectionAcceptable realSolution t dotLookAtAndTracingRay then
            let (doesRayContribute,outRay,scatteredShading) = surface.Scatter ray t 0
            if writeToDepth then writeToDepthBuffer t px py
            let emittedShading = surface.Emitted
            match doesRayContribute with
                | true -> emittedShading + scatteredShading*(rayTrace outRay 1us)
                | false -> emittedShading
        else
            backgroundColor

    let pertrube px py = 
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt
        let randVec = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
        let xOff = MathF.Abs(randVec.X)/((float32)(width))
        let yOff = MathF.Abs(randVec.Y)/((float32)(height))
        // let xOff = MathF.Abs(randVec.X)/(0.5f)
        // let yOff = MathF.Abs(randVec.Y)/(0.5f)
        ((float32)px + xOff, (float32)py+yOff)

    [<Benchmark>]
    member self.renderScene () =
        for px in 0..width-1 do
            for py in 0..height-1 do
                // offset pixels to achieve antialiasing
                let (pxOffset,pyOffset) = pertrube px py
                let dirCS = 
                    rayDirection (pixelToCamera pxOffset pyOffset (float32 width) (float32 height) fov)
                let rot = rotation cameraWS
                let dirWS = Vector3.Normalize(Vector3.TransformNormal(dirCS,rot))
                let ray = Ray(cameraWS.Translation, dirWS)
                let color = rayTraceBase ray px py true
                let colors = color :: List.init (samples-1) (fun _ -> rayTraceBase ray px py false)
                let avgColor = (List.reduce (fun acc c -> acc+c) colors)/(float32)samples
                //Gamma correct TODO: refactor
                frameBuffer.[px,py] <- Vector4(Vector3.SquareRoot(avgColor),1.0f)


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