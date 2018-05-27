module Raytracer.Surface

open System
open System.Numerics
open Raytracer.Geometry
open Raytracer.Material
open Henzai.Sampling
open Raytracer.Numerics
open Raytracer
open Henzai.Geometry

type ID = uint64

let randomState = new Random()
        
[<AbstractClass>]
type Surface(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =
    abstract member Scatter: Ray -> LineParameter -> int -> bool*Ray*Raytracer.Material.Color
    abstract member Emitted : Material.Color 
    member this.ID = id
    member this.Geometry = geometry
    member this.Material = material
    default this.Scatter _ _ _ = (true,Ray(Vector3.UnitX,Vector3.UnitX),Vector3.UnitY)
    default this.Emitted = this.Material.Emmitance

//let ToSurface x = upcast x : Surface


type NoSurface(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =
    inherit Surface(id, geometry, material)

// TODO refactor into geometry class
let findClosestIntersection (ray : Ray) (surfaces : Surface list) =
    let allIntersections = List.map flattenIntersection (List.map (fun (x : Surface) -> (x.Geometry.Intersect ray),x) surfaces)
    let allIntersectionsWithRealSolutions = List.filter (fun ((b,t,v) : bool*LineParameter*Surface) ->  v.Geometry.IntersectionAcceptable b t 1.0f (PointForRay ray t)) allIntersections
    let closestIntersection : bool*LineParameter*Surface = 
        match allIntersectionsWithRealSolutions with 
            | [] -> (false,0.0f, upcast (NoSurface(0UL,NotHitable(),Raytracer.Material.Material(Vector3.Zero))))
            | realSolutions -> List.reduce (fun smallest current -> smallestIntersection smallest current) realSolutions
    closestIntersection

// type Emitting(id: ID, geometry : Hitable, material : Raytracer.Material.Material)  =
//     inherit Surface(id,geometry,material) with 
//         override this.Emitted = this.Material.Albedo

type Lambertian(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =
    inherit Surface(id,geometry,material)

    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt

        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let normal = this.Geometry.NormalForSurfacePoint positionOnSurface

        //TODO sample hemisphere
        let rand_norm = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
        let outDir = Vector3.Normalize(normal+rand_norm)
        //let outDir = Vector3.Normalize(normal)
        let outRay = Ray(positionOnSurface,outDir,this.ID)
        let doesRayContribute = not (geometry.IsObstructedBySelf outRay)
        let attenuation = this.Material.Albedo
        // let light = normal
        let attenuationDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*attenuation
        (doesRayContribute,outRay,attenuationDepthAdjusted)



type Metal(id: ID, geometry : Hitable, material : Raytracer.Material.Material, fuzz : float32) =
    inherit Surface(id,geometry,material)

    member this.Fuzz = MathF.Max(MathF.Min(1.0f,fuzz),0.0f)
    member this.Reflect (incommingRay : Ray) (normalToSurface : Normal) 
        = incommingRay.Direction - 2.0f*Vector3.Dot(incommingRay.Direction,normalToSurface)*normalToSurface 

    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =

        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt
        let rand_norm = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)

        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let normal = Vector3.Normalize(this.Geometry.NormalForSurfacePoint positionOnSurface + this.Fuzz*rand_norm)

        let outDir = Vector3.Normalize(this.Reflect incommingRay normal)
        let outRay =  Ray(positionOnSurface,outDir,this.ID)    
        let isObstructedBySelf = (this.Geometry.IsObstructedBySelf outRay)
        let doesRayContribute = (not isObstructedBySelf)
        let attenuation = material.Albedo
        // let lightDepthAdjusted = applyFuncToVector3 (power (1.0f/(float32)depthLevel) ) light
        let attenuationDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*attenuation
        (doesRayContribute,outRay,attenuationDepthAdjusted)

type Dielectric(id: ID, geometry : Hitable, material : Raytracer.Material.Material, refractiveIndex : float32) =
    inherit Surface(id,geometry,material)

    member this.RefractiveIndex = refractiveIndex

    //TODO: finish this
    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =
        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let normal = Vector3.Normalize(this.Geometry.NormalForSurfacePoint positionOnSurface)
        let refrativeIndexAir = 1.0f
        // transmition over incidence
        let (refractiveIndexRatio,fresnelNormal)
            = if Vector3.Dot(incommingRay.Direction,normal) > 0.0f then 
                refrativeIndexAir/this.RefractiveIndex, -normal
              else this.RefractiveIndex , normal
        let attenuation = material.Albedo
        let attenuationDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*attenuation
        (false,incommingRay,attenuationDepthAdjusted)



//https://learnopengl.com/Lighting/Light-casters
//TODO refactor constants
let attenuate distance = 1.0f/(1.0f + 0.5f*distance + 0.02f*(distance**2.0f))
let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfaceWithId (surfaces : Surface list) (id : ID) =
    List.head (List.filter (fun (surface : Surface) -> surface.ID = id) surfaces)

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces






