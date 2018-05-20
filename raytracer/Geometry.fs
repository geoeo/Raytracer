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
//type Sphere = Origin * Radius
//type Plane = Normal * Offset

type Ray =
    struct
        val Origin : Vector3
        val Direction : Vector3

        new(origin, dir) = { Origin = origin; Direction = normalized(dir) }

    end


type Sphere(sphereCenter : Origin,radius : Radius) =
    member this.Center = sphereCenter
    member this.Radius = radius


let hasIntersection (hasIntersection : bool,_,_) = hasIntersection

//let hasIntersection (hasIntersection: bool,_) = hasIntersection

let getIntersections (_,i1,i2) = (i1,i2)

let intersectSphere (t : float32) (ray : Ray) (sphere : Sphere) =
    ((ray.Origin + t*ray.Direction) - sphere.Center).Length() <= sphere.Radius

//TODO: Refactor to class
let sphereIntersections (sphere : Sphere) (ray : Ray) =
    let centerToRay = ray.Origin - sphere.Center
    let dirDotCenterToRay = Vector3.Dot(ray.Direction ,centerToRay)
    let discriminant = 
        MathF.Pow(dirDotCenterToRay, 2.0f) - centerToRay.LengthSquared() + sphere.Radius**2.0f
    if discriminant < 0.0f then (false, -1.0f,-1.0f)
    else if round discriminant 1 = 0.0f then (true,-dirDotCenterToRay,-1.0f)
    else (true,-dirDotCenterToRay + MathF.Sqrt(discriminant),-dirDotCenterToRay - MathF.Sqrt(discriminant))

let sphereClosestIntersection (sphere: Sphere) (ray : Ray) =
     smallestNonNegative (getIntersections (sphereIntersections (sphere : Sphere) (ray : Ray)))

let sphereNormal (positionOnSphere:Vector3) (center:Vector3) =
    Vector3.Normalize(positionOnSphere - center)

 //TODO: Refactor to class
let planeIntersection (plane : Plane) (ray : Ray) =
    let numerator = -plane.D - Plane.DotNormal(plane,ray.Origin) 
    let denominator = Plane.DotNormal(plane,ray.Direction)
    if Math.Abs(round denominator 2) < 0.01f  then (false, 0.0f)
    else (true, numerator / denominator)

let isRayObstructed (spheres : Sphere list ) (ray : Ray) =
    let intersections = 
        seq { for sphere in spheres do yield hasIntersection (sphereIntersections sphere ray)}
    Seq.fold (fun b1 b2 -> b1 || b2) false intersections

