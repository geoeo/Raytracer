module Geometry

open System.Numerics

type Origin = Vector3 // Position of a point in 3D space
type Direction = Vector3 // Direction vector (normalized)
type Radius = float32
type Parameter = float32

type Ray = Origin * Direction
type Sphere = Origin * Radius

let intersectSphere (t : Parameter) ((rayOrigin,dir) : Ray) ((sphereCenter,radius) : Sphere) =
    ((rayOrigin + t*dir) - sphereCenter).Length() <= radius