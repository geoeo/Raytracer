module Geometry

open System.Numerics
open GeometryNumerics



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



let intersectSphere (t : float32) (ray : Ray) ((sphereCenter,radius) : Sphere) =
    ((ray.Origin + t*ray.Direction) - sphereCenter).Length() <= radius
