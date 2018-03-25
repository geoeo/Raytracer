module Geometry

open System
open System.Numerics
open Numerics



type Origin = Vector3 // Position of a point in 3D space
type Direction = Vector3    
type Radius = float32
type Parameter = float32
type Sphere = Origin * Radius

type Ray =
    struct
        val Origin : Vector3
        val Direction : Vector3

        new(origin, dir) = { Origin = origin; Direction = normalized(dir) }

    end

let magnitudeSquared (vec3 : Vector3) = vec3.X*vec3.X + vec3.Y*vec3.Y + vec3.Z*vec3.Z

let intersectSphere (t : float32) (ray : Ray) ((sphereCenter,radius) : Sphere) =
    ((ray.Origin + t*ray.Direction) - sphereCenter).Length() <= radius

let sphereIntersections (ray : Ray) ((sphereCenter,radius) : Sphere) =
    let centerToRay = ray.Origin - sphereCenter
    let dirDotCenterToRay = Vector3.Dot(ray.Direction ,centerToRay)
    let discriminant = 
        MathF.Pow(dirDotCenterToRay, 2.0f) - magnitudeSquared(centerToRay) + radius*radius
    if discriminant < 0.0f then (false, -1.0f,-1.0f)
    else if round discriminant 1 = 0.0f then (true,-dirDotCenterToRay,-1.0f)
    else (true,-dirDotCenterToRay + MathF.Sqrt(discriminant),-dirDotCenterToRay - MathF.Sqrt(discriminant))

