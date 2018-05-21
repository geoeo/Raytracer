module Raytracer.Surface

open System
open System.Numerics
open Raytracer.Geometry
open Raytracer.Material
open Henzai.Sampling
open Raytracer.Numerics

type ID = uint64

let randomState = new Random()

let attenuate distance = 1.0f/(1.0f + 0.045f*distance + 0.0007f*(distance**2.0f))

let LightTransport isObstructed =
    match isObstructed with
        | true -> 0.0f
        | false -> 1.0f

type Surface(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =
    abstract member Scatter: Ray -> Point -> Point -> Hitable list -> bool*Ray*Raytracer.Material.Color

    static member ToSurface x = x :> Surface 

    default this.Scatter _ _ _ _ = (false,Ray(Vector3.Zero,Vector3.Zero),this.Material.Color)

    member this.ID = id
    member this.Geometry = geometry
    member this.Material : Raytracer.Material.Material = material

type Lambertian(id: ID, geometry : Hitable, material : Raytracer.Material.Material) =
    inherit Surface(id,geometry,material)

    override this.Scatter (incommingRay : Ray) (positionOnSurface : Point) (lightWS: Point) (allOtherGeometries : Hitable list) =
        let randomInt = randomState.Next()
        let randomUnsingedInt : uint32 = (uint32) randomInt

        let normal = this.Geometry.NormalForSurfacePoint positionOnSurface
        let pointToLight = lightWS - positionOnSurface
        let pointToLightNorm = Vector3.Normalize(pointToLight)
        let rayHitToLight = Ray(positionOnSurface,pointToLightNorm,this.ID)

        //let lightTransport = LightTransport(IsRayObstructed allOtherGeometries rayHitToLight lightWS)

        //TODO sample hemisphere
        let rand_norm = RandomSampling.RandomInUnitSphere(ref randomUnsingedInt)
        // let outDir = Vector3.Normalize(normal+rand_norm)
        let outDir = Vector3.Normalize(normal)
        let outRay = Ray(positionOnSurface,outDir,this.ID)
        let diffuseFactor = Vector3.Dot(normal,pointToLightNorm)
        let atteunuation = attenuate (pointToLight.Length())
        let isObstructedBySelf = (this.Geometry.IsObstructedBySelf rayHitToLight)
        let doesRayContribute = (not (IsRayObstructed allOtherGeometries rayHitToLight lightWS)) && (not isObstructedBySelf)
        (doesRayContribute,outRay,atteunuation*diffuseFactor*this.Material.Color)



let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfaceWithId (surfaces : Surface list) (id : ID) =
    List.head (List.filter (fun (surface : Surface) -> surface.ID = id) surfaces)

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces

//https://learnopengl.com/Lighting/Light-casters
//TODO refactor constants

// TODO refactor into geometry class
let findClosestIntersection (ray : Ray) (surfaces : Surface list) =
    let allIntersections = List.map flattenIntersection (List.map (fun (x : Surface) -> (x.Geometry.Intersect ray),x) surfaces)
    let allIntersectionsWithRealSolutions = List.filter (fun (b,t,v) -> b) allIntersections
    match allIntersectionsWithRealSolutions with 
        | [] -> (false,0.0f,Surface(((uint64)0,Hitable(),Raytracer.Material.Material(Vector3.Zero))))
        | _ -> List.reduce (fun smallest current -> smallestIntersection smallest current) allIntersectionsWithRealSolutions





