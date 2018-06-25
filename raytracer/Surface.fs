module Raytracer.Surface

open System
open System.Numerics
open Raytracer.Geometry
//open Raytracer.Materials
open Henzai.Numerics
open Raytracer.Numerics
open Raytracer

type ID = uint64

let randomState = new Random()
        
[<AbstractClass>]
type Surface(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =
    abstract member Scatter: Ray -> LineParameter -> int -> bool*Ray*Angle
    abstract member Emitted : Material.Color 
    abstract member SampleCount : int
    abstract member PDF : float32
    abstract member BRDF : Raytracer.Material.Color
    abstract member GenerateSamples : Ray -> LineParameter -> int -> (Ray* Material.Color)[]
    member this.ID = id
    member this.Geometry = geometry
    member this.Material = material
    member this.MCNormalization = MathF.Max((float32)this.SampleCount, 1.0f)

    default this.Scatter _ _ _ = (true,Ray(Vector3.UnitX,Vector3.UnitX),1.0f)
    default this.Emitted = this.Material.Emmitance
    default this.SampleCount = 0
    default this.PDF = 1.0f
    default this.BRDF = this.Material.Albedo
    //TODO: Preallocate Memory to avoid runtime allocation
    default this.GenerateSamples (incommingRay : Ray) (t : LineParameter) (depthLevel : int) = 
        [|
            for _ in 1..this.SampleCount do
                let (doesRayContribute,outRay,cosOfIncidence) = this.Scatter incommingRay t (depthLevel)
                let shading : Material.Color = this.BRDF*cosOfIncidence / (this.PDF*this.MCNormalization)
                yield (outRay , shading)
        |]


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

    override this.SampleCount = 6
    override this.PDF = 1.0f / (2.0f * MathF.PI)
    override this.BRDF = this.Material.Albedo / MathF.PI
    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =

        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let mutable normal = this.Geometry.NormalForSurfacePoint positionOnSurface

        //sampling hemisphere
        let rand_norm = RandomSampling.RandomInUnitHemisphere_Sync()
        let cosOfIncidence = rand_norm.Y
        let mutable nb = Vector3.Zero
        let mutable nt = Vector3.Zero
        Henzai.Numerics.Geometry.CreateCoordinateSystemAroundNormal(&normal,&nt,&nb)
        let changeOfBaseMatrix = ChangeOfBase &nt &normal &nb
        let normalSample = Vector4.Transform(rand_norm,changeOfBaseMatrix)
        let outDir = Vector3.Normalize(ToVec3 normalSample)

        //let outDir = Vector3.Normalize(normal)
        let outRay = Ray(positionOnSurface,outDir,this.ID)
        (true,outRay,cosOfIncidence)



type Metal(id: ID, geometry : Hitable, material : Raytracer.Material.Material, fuzz : float32) =
    inherit Surface(id,geometry,material)

    member this.Fuzz = MathF.Max(MathF.Min(1.0f,fuzz),0.0f)
    member this.Reflect (incommingRay : Ray) (normalToSurface : Normal) 
        = incommingRay.Direction - 2.0f*Vector3.Dot(incommingRay.Direction,normalToSurface)*normalToSurface 

    override this.SampleCount = 1
    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =

        let positionOnSurface = incommingRay.Origin + t*incommingRay.Direction
        let mutable normal = Vector3.Normalize(this.Geometry.NormalForSurfacePoint positionOnSurface)

        //sampling hemisphere
        let rand_norm = RandomSampling.RandomInUnitHemisphere_Sync()
        let mutable nb = Vector3.Zero
        let mutable nt = Vector3.Zero
        Henzai.Numerics.Geometry.CreateCoordinateSystemAroundNormal(&normal,&nt,&nb)
        let changeOfBaseMatrix = ChangeOfBase &nt &normal &nb
        let normalSample = ToVec3 (Vector4.Transform(rand_norm,changeOfBaseMatrix))
        let modifiedNormal = Vector3.Normalize((1.0f - this.Fuzz)*normal + this.Fuzz*normalSample)

        let outDir = Vector3.Normalize(this.Reflect incommingRay modifiedNormal)
        let outRay =  Ray(positionOnSurface,outDir,this.ID)    
        (true,outRay,1.0f)

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
    override this.SampleCount = 1
    ///<summary>
    /// Returns: (Reflect Probability,intersection Position,Reflection Dir, Refraction Dir)
    /// </summary>
    member this.CalcFresnel (incommingRay : Ray) (t : LineParameter) (depthLevel : int) = 
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
        (reflectProb , positionOnSurface, reflectDir, refrationDir)

    override this.Scatter (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =
        let (reflectProb, positionOnSurface, reflectDir, refractionDir) = this.CalcFresnel incommingRay t depthLevel
        let randomFloat = RandomSampling.RandomFloat_Sync()
        if randomFloat <= reflectProb 
        then 
            let reflectRay = Ray(positionOnSurface,reflectDir,this.ID)
            // (true,reflectRay,attenuationDepthAdjusted)
            (true,reflectRay,1.0f)
        else // refraction has to have been successful
            let refractRay = Ray(positionOnSurface,refractionDir)
            // (true,refractRay,attenuationDepthAdjusted)
            (true,refractRay,1.0f)
    override this.GenerateSamples (incommingRay : Ray) (t : LineParameter) (depthLevel : int) =
        let (reflectProb, positionOnSurface, reflectDir, refractionDir) = this.CalcFresnel incommingRay t depthLevel
        let reflectRay = Ray(positionOnSurface,reflectDir,this.ID)
        let reflectShading : Material.Color = this.BRDF*reflectProb // / (this.PDF*this.MCNormalization)
        if reflectProb = 1.0f then //TODO: Maybe round
            [|(reflectRay,reflectShading)|]
        else
            let refractRay = Ray(positionOnSurface,refractionDir)
            let refractShading : Material.Color = this.BRDF*(1.0f - reflectProb) // / (this.PDF*this.MCNormalization)
            [|(reflectRay,reflectShading);(refractRay, refractShading)|]



//https://learnopengl.com/Lighting/Light-casters
//TODO refactor constants
let attenuate distance = 1.0f/(1.0f + 0.5f*distance + 0.02f*(distance*distance))
let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfaceWithId (surfaces : Surface list) (id : ID) =
    List.head (List.filter (fun (surface : Surface) -> surface.ID = id) surfaces)

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces