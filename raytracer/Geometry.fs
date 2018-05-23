module Raytracer.Geometry

open System
open System.Numerics
open Raytracer.Numerics

type Origin = Vector3 // Position of a point in 3D space
type Direction = Vector3  
type Normal = Vector3
type Offset = float32  
type Radius = float32
type LineParameter = float32
type Point = Vector3

type Ray =
    struct
        val Origin : Vector3
        val Direction : Vector3
        val SurfaceOrigin : uint64

        new(origin, dir) = { Origin = origin; Direction = normalized(dir);SurfaceOrigin = (uint64)0 }
        new(origin, dir, id) = { Origin = origin; Direction = normalized(dir);SurfaceOrigin = id }

    end

type Hitable =
    abstract member HasIntersection: Ray -> bool
    abstract member Intersect: Ray -> bool*LineParameter
    abstract member IntersectionAcceptable : bool -> LineParameter -> float32  -> bool
    abstract member NormalForSurfacePoint : Point -> Normal
    abstract member IsObstructedBySelf: Ray -> bool 
    abstract member TMin : float32
    abstract member TMax : float32


let ToHitable x = x:> Hitable 

// Does the ray penetrating the surface have a t < tCompare
let IsIntersectionInfrontOf (geometry : Hitable) (ray : Ray) (tCompare : LineParameter) = 
        let (hasIntersections,t) = geometry.Intersect ray
        if hasIntersections && t > geometry.TMin then t < tCompare else false


[<Struct>]
type NotHitable =
    interface Hitable with

    // effects shadow acne
    member this.TMin = 0.0000f
    member this.TMax = 0.0f
    member this.HasIntersection _ = false
    member this.Intersect _ = (false, 0.0f)
    member this.IntersectionAcceptable _ _ _ = false
    member this.NormalForSurfacePoint _ = Vector3.Zero
    member this.IsObstructedBySelf _ = false


[<Struct>]
type Sphere(sphereCenter : Origin,radius : Radius) =

    member this.Center = sphereCenter
    member this.Radius = radius
    member this.GetIntersections (_,i1,i2) = (i1,i2)
    member this.IntersectWith (t : LineParameter) (ray : Ray) =
        ((ray.Origin + t*ray.Direction) - this.Center).Length() <= this.Radius

    member this.Intersections (ray : Ray) = 
        let centerToRay = ray.Origin - this.Center
        let dirDotCenterToRay = Vector3.Dot(ray.Direction ,centerToRay)
        let discriminant = 
            MathF.Pow(dirDotCenterToRay, 2.0f) - centerToRay.LengthSquared() + this.Radius**2.0f
        if round discriminant 5 < 0.0f then (false, 0.0f,0.0f)
        // TODO: may cause alsiasing investigate around sphere edges
        else if round discriminant 5 = 0.0f then (true,-dirDotCenterToRay,System.Single.MinValue)
        else (true,-dirDotCenterToRay + MathF.Sqrt(discriminant),-dirDotCenterToRay - MathF.Sqrt(discriminant))

    interface Hitable with
        member this.TMin = 0.0001f
        member this.TMax = 500.0f
        member this.Intersect (ray : Ray) = 
            let (hasIntersection,i1,i2) = this.Intersections (ray : Ray)
            (hasIntersection,MathF.Min(i1,i2))
        member this.NormalForSurfacePoint (positionOnSphere:Point) =
            Vector3.Normalize(positionOnSphere - this.Center)
        member this.HasIntersection ray =
            let (hasIntersection,_,_) = this.Intersections ray 
            hasIntersection
        member this.IntersectionAcceptable hasIntersection t _ =
            hasIntersection && t > (this :> Hitable).TMin
        member this.IsObstructedBySelf ray =
            let (b,i1,i2) = this.Intersections ray
            (this :> Hitable).IntersectionAcceptable b (MathF.Max(i1,i2)) 1.0f


[<Struct>]
//TODO enforce boundaries so that it is no longer infinite
type Plane(plane : System.Numerics.Plane) = 
    member this.Plane = plane
    member this.Normal = this.Plane.Normal
    interface Hitable with
        member this.TMin = 0.0001f
        member this.TMax = 500.0f
        member this.Intersect (ray:Ray) =
            let numerator = -this.Plane.D - Plane.DotNormal(this.Plane,ray.Origin) 
            let denominator = Plane.DotNormal(this.Plane,ray.Direction)
            if Math.Abs(round denominator 2) < 0.01f  then (false, 0.0f)
            else (true, numerator / denominator)
        member this.HasIntersection (ray:Ray) = 
            let (hasIntersection,_) = (this :> Hitable).Intersect ray 
            hasIntersection
        // dotView factor ensures sampling "straight" at very large distances due to fov
        member this.IntersectionAcceptable hasIntersection t dotViewTrace =
            hasIntersection && t > (this :> Hitable).TMin && t <= ((this :> Hitable).TMax/dotViewTrace)
        member this.NormalForSurfacePoint _ =
            this.Normal

        member this.IsObstructedBySelf _ = false

let ParameterToPointForRay (ray : Ray) (point : Point) =
    if ray.Direction.X = 0.0f then (point.Y - ray.Origin.Y)/ray.Direction.Y
    else (point.X - ray.Origin.X)/ray.Direction.X

let DoesRayTransportLight (surfaces : Hitable list) (ray : Ray) (light: Hitable) = 
    // let t_HitLight = ParameterToPointForRay ray lightWS
    let (b,t_HitLight) = light.Intersect ray
    if light.IntersectionAcceptable b t_HitLight 1.0f then 
        let intersections = 
            seq { for surface in surfaces do yield (not (IsIntersectionInfrontOf surface ray t_HitLight))}
        Seq.fold (fun b1 b2 -> b1 && b2) true intersections
    else false

let smallestIntersection (b,t,x) (b_new,t_new,x_new) =
    if t <= t_new then (b,t,x)
    else (b_new,t_new,x_new)

let flattenIntersection ((b,t),x) = (b,t,x)

let distanceOfIntersection (ray : Ray) (t : LineParameter) =
    let line = (ray.Origin + t*ray.Direction)
    line.Length()

       

