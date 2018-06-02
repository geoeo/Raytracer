module Raytracer.Surface

open System
open System.Numerics
open Raytracer.Geometry
//open Raytracer.Materials
open Henzai.Sampling
open Raytracer.Numerics
open Raytracer

type ID = uint64

// TODO investigate this beheviour for multithreading
let randomState = new Random()
//let randomSampler = new RandomSampling()
        
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

let findClosestIntersection (ray : Ray) (surfaces : Surface array) =
    let mutable (bMin,tMin,vMin : Surface) = (false,Single.MaxValue, upcast (NoSurface(0UL,NotHitable(),Raytracer.Material.Material(Vector3.Zero))))
    for surface in surfaces do
        let (b,t) = surface.Geometry.Intersect ray
        if surface.Geometry.IntersectionAcceptable b t 1.0f (PointForRay ray t) &&  t < tMin then
            bMin <- b
            tMin <- t
            vMin <- surface

    (bMin,tMin,vMin)

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
        let attenuation = this.Material.Albedo
        let attenuationDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*attenuation
        (true,outRay,attenuationDepthAdjusted)



type Metal(id: ID, geometry : Hitable, material : Raytracer.Material.Material, fuzz : float32) =
    inherit Surface(id,geometry,material)

    member this.Fuzz = MathF.Max(MathF.Min(1.0f,fuzz),0.0f)
    member this.Reflect (incommingRay : Ray) (normalToSurface : Normal) 
        = incommingRay.Direction - 2.0f*Vector3.Dot(incommingRay.Direction,normalToSurface)*normalToSurface 

    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt
        let rand_norm = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
        //let rand_norm = Vector3.UnitX

        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let normal = Vector3.Normalize(this.Geometry.NormalForSurfacePoint positionOnSurface + this.Fuzz*rand_norm)

        let outDir = Vector3.Normalize(this.Reflect incommingRay normal)
        let outRay =  Ray(positionOnSurface,outDir,this.ID)    
        let attenuation = material.Albedo
        let attenuationDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*attenuation
        (true,outRay,attenuationDepthAdjusted)

// https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading/reflection-refraction-fresnel
type Dielectric(id: ID, geometry : Hitable, material : Raytracer.Material.Material, refractiveIndex : float32) =
    inherit Surface(id,geometry,material)

    member this.RefractiveIndex = refractiveIndex

    //https://seblagarde.wordpress.com/2013/04/29/memo-on-fresnel-equations/
    member this.SchlickApprx (cos_incidence : float32) (refractiveIncidenceFactor : float32) (refractiveTransmissionFactor : float32) =
        let R0 = MathF.Pow((refractiveTransmissionFactor-refractiveIncidenceFactor)/(refractiveTransmissionFactor+refractiveIncidenceFactor),2.0f)
        R0 + (1.0f - R0)*MathF.Pow((1.0f - cos_incidence),5.0f)

    member this.Reflect (incommingRay : Ray) (normalToSurface : Normal) 
        = incommingRay.Direction - 2.0f*Vector3.Dot(incommingRay.Direction,normalToSurface)*normalToSurface 

    member this.Refract (incommingDirection : Direction) (normalToSurface : Normal) (refractiveIncidenceOverTransmission : float32) (cos_incidence : float32) =
        let discriminant = 1.0f - (Square refractiveIncidenceOverTransmission)*(1.0f - Square cos_incidence)
        if discriminant > 0.0f then 
            let refracted = refractiveIncidenceOverTransmission*(incommingDirection + cos_incidence*normalToSurface) - normalToSurface*MathF.Sqrt(discriminant)
            (true, Vector3.Normalize(refracted))
        // total internal refleciton
        else (false, Vector3.Zero) 

    //TODO: investigate refraction artefacts
    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt
        let randomFloat = RandomSampling.RandomFloat(ref randomUnsingedInt)
        //let randomFloat = 0.5f

        let attenuation = material.Albedo
        let attenuationDepthAdjusted = MathF.Pow(0.95f,(float32)depthLevel)*attenuation

        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let normal = Vector3.Normalize(this.Geometry.NormalForSurfacePoint positionOnSurface)
        let reflectDir = Vector3.Normalize(this.Reflect incommingRay normal)
        let refrativeIndexAir = 1.0f

        //incidence over transmition
        let (incidenceIndex, transmissionIndex,fresnelNormal)
            // Vector is "comming out" of material into air
            = if Vector3.Dot(incommingRay.Direction,normal) > 0.0f 
              then 
                this.RefractiveIndex,refrativeIndexAir, -normal
              else 
                refrativeIndexAir, this.RefractiveIndex , normal
        let cos_incidence =  Vector3.Dot(incommingRay.Direction,-fresnelNormal)
        let (refracted,refrationDir) = this.Refract incommingRay.Direction fresnelNormal (incidenceIndex/transmissionIndex) cos_incidence
        //Use schlick if refraction was successful
        let reflectProb = if refracted then this.SchlickApprx cos_incidence incidenceIndex transmissionIndex  else 1.0f

        if randomFloat <= reflectProb 
        then 
            let reflectRay = Ray(positionOnSurface,reflectDir,this.ID)
            (true,reflectRay,attenuationDepthAdjusted)
        else // refraction has to have been successful
            let refractRay = Ray(positionOnSurface,refrationDir)
            (true,refractRay,attenuationDepthAdjusted)



//https://learnopengl.com/Lighting/Light-casters
//TODO refactor constants
let attenuate distance = 1.0f/(1.0f + 0.5f*distance + 0.02f*(distance*distance))
let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfaceWithId (surfaces : Surface list) (id : ID) =
    List.head (List.filter (fun (surface : Surface) -> surface.ID = id) surfaces)

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces






