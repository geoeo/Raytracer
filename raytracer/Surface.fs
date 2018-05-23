module Raytracer.Surface

open System
open System.Numerics
open Raytracer.Geometry
open Raytracer.Material
open Henzai.Sampling
open Raytracer.Numerics

type ID = uint64

let randomState = new Random()

//https://learnopengl.com/Lighting/Light-casters
//TODO refactor constants
let attenuate distance = 1.0f/(1.0f + 0.5f*distance + 0.02f*(distance**2.0f))

let LightTransport isObstructed =
    match isObstructed with
        | true -> 0.0f
        | false -> 1.0f
        


type Surface =
        abstract member Scatter: Ray -> LineParameter -> Sphere -> Hitable list -> int -> bool*Ray*Raytracer.Material.Color
        abstract member ID : ID
        abstract member Geometry : Hitable
        abstract member Material : Material

let ToSurface x = x :> Surface 

[<Struct>]
type NoSurface(id: ID, geometry : Hitable, material : Material) =

    interface Surface with
        member this.ID = id
        member this.Geometry = geometry
        member this.Material = material
        member this.Scatter _ _ _ _ _ = (false,Ray(Vector3.Zero,Vector3.Zero),material.Color)

[<Struct>]
type Lambertian(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =

    interface Surface with
        member this.ID = id
        member this.Geometry = geometry
        member this.Material = material

        member this.Scatter (incommingRay : Ray) (t : LineParameter) (light: Sphere) (allOtherGeometries : Hitable list) (depthLevel : int) =
            let randomInt = randomState.Next()
            let randomUnsingedInt : uint32 = (uint32) randomInt

            let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
            let normal = geometry.NormalForSurfacePoint positionOnSurface

            //TODO sample hemisphere
            let rand_norm = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
            let outDir = Vector3.Normalize(normal+rand_norm)
            // let outDir = Vector3.Normalize(normal)
            let outRay = Ray(positionOnSurface,outDir,id)
            let (b,t) = light.Intersect outRay
            //let atteunuationForLight = attenuate (pointToLight.Length())
            let atteunuationForRay = attenuate t
            let isObstructedBySelf = (geometry.IsObstructedBySelf outRay)
            let doesRayContribute = (light.IntersectionAcceptable b t 1.0f) && (not (IsRayObstructed allOtherGeometries outRay light.Center)) && (not isObstructedBySelf)
            let light = material.Color
            // let lightDepthAdjusted = applyFuncToVector3 (power (1.0f/(float32)depthLevel) ) light
            let lightDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*light
            (doesRayContribute,outRay,lightDepthAdjusted)




let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfaceWithId (surfaces : Surface list) (id : ID) =
    List.head (List.filter (fun (surface : Surface) -> surface.ID = id) surfaces)

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces

// TODO refactor into geometry class
let findClosestIntersection (ray : Ray) (surfaces : Surface list) =
    let allIntersections = List.map flattenIntersection (List.map (fun (x : Surface) -> (x.Geometry.Intersect ray),x) surfaces)
    let allIntersectionsWithRealSolutions = List.filter (fun (b,t,v) -> b) allIntersections
    match allIntersectionsWithRealSolutions with 
        | [] -> (false,0.0f, NoSurface(0UL,Hitable(),Material(Vector3.Zero)) :> Surface)
        | _ -> List.reduce (fun smallest current -> smallestIntersection smallest current) allIntersectionsWithRealSolutions





