module Raytracer.Geometry

open System
open System.Numerics
open Raytracer.Numerics

type Origin = Vector3 // Position of a point in 3D space
type Direction = Vector3  
type Normal = Vector3
type Offset = float32  
type Radius = float32
type Parameter = float32
//TODO put in abstract class
let getIntersections (_,i1,i2) = (i1,i2)

//let hasIntersection (hasIntersection : bool,_,_) = hasIntersection

//let hasIntersection (hasIntersection: bool,_) = hasIntersection

type Ray =
    struct
        val Origin : Vector3
        val Direction : Vector3

        new(origin, dir) = { Origin = origin; Direction = normalized(dir) }

    end

type Hitable() =
    abstract member HasIntersection: Ray -> bool

    default this.HasIntersection _ = false

type Sphere(sphereCenter : Origin,radius : Radius) =
    inherit Hitable()
    member this.Center = sphereCenter
    member this.Radius = radius
    member this.Intersect (ray : Ray) = 
        let centerToRay = ray.Origin - this.Center
        let dirDotCenterToRay = Vector3.Dot(ray.Direction ,centerToRay)
        let discriminant = 
            MathF.Pow(dirDotCenterToRay, 2.0f) - centerToRay.LengthSquared() + this.Radius**2.0f
        if discriminant < 0.0f then (false, -1.0f,-1.0f)
        else if round discriminant 1 = 0.0f then (true,-dirDotCenterToRay,-1.0f)
        else (true,-dirDotCenterToRay + MathF.Sqrt(discriminant),-dirDotCenterToRay - MathF.Sqrt(discriminant))
    member this.ClosestIntersection (ray : Ray) = 
        smallestNonNegative (getIntersections (this.Intersect (ray : Ray)))
    member this.IntersectWith (t : float32) (ray : Ray) =
        ((ray.Origin + t*ray.Direction) - this.Center).Length() <= this.Radius
    member this.NormalToPointOnSphere (positionOnSphere:Vector3) =
        Vector3.Normalize(positionOnSphere - this.Center)
    override this.HasIntersection (ray : Ray) = 
        let (hasIntersection,_,_) = this.Intersect ray 
        hasIntersection

type Plane(plane : System.Numerics.Plane) = 
    inherit Hitable()
    member this.Plane = plane
    member this.Normal = this.Plane.Normal
    member this.Intersect (ray:Ray) =
        let numerator = -this.Plane.D - Plane.DotNormal(this.Plane,ray.Origin) 
        let denominator = Plane.DotNormal(this.Plane,ray.Direction)
        if Math.Abs(round denominator 2) < 0.01f  then (false, 0.0f)
        else (true, numerator / denominator)
    override this.HasIntersection (ray:Ray) = 
        let (hasIntersection,_) = this.Intersect ray 
        hasIntersection


let isRayObstructed (spheres : Sphere list ) (ray : Ray) =
    let intersections = 
        seq { for sphere in spheres do yield sphere.HasIntersection ray}
    Seq.fold (fun b1 b2 -> b1 || b2) false intersections

