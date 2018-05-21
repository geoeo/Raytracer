module Raytracer.Surface

open System
open System.Numerics
open Raytracer.Geometry
open Raytracer.Material
open Henzai.Sampling
open System.Collections.Generic

type ID = uint64

let randomState = new Random()


let LightTransport isObstructed =
    match isObstructed with
        | true -> 0.0f
        | false -> 1.0f

[<AbstractClass>]
type Surface(id: ID, geometry : Hitable, material : Material) =
    abstract member Scatter: Ray -> Point -> Point -> Hitable list -> bool*Ray*Raytracer.Material.Color

    static member ToSurface x = x :> Surface 
    member this.ID = id
    member this.Geometry = geometry
    member this.Material = material

type Lambertian(id: ID, geometry : Hitable, material : Material) =
    inherit Surface(id,geometry,material)

    override this.Scatter (incommingRay : Ray) (positionOnSurface : Point) (lightWS: Point) (allOtherGeometries : Hitable list) =
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt

        let normal = this.Geometry.NormalForSurfacePoint positionOnSurface
        let pointToLight = Vector3.Normalize(lightWS - positionOnSurface)
        let rayHitToLight = Ray(positionOnSurface,pointToLight)

        let lightTransport = LightTransport(IsRayObstructed allOtherGeometries rayHitToLight lightWS)

        //TODO sample hemisphere
        let rand_norm = 0.8f*RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
        let outDir = Vector3.Normalize(normal+ rand_norm)
        // let outDir = Vector3.Normalize(normal)
        let outRay = Ray(positionOnSurface,outDir)
        let diffuseFactor = Vector3.Dot(normal,pointToLight)
        (true,outRay,lightTransport*diffuseFactor*this.Material.Color)



let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces





