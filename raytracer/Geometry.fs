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

/// Space inwhich points are compared if they are inside a rectangle
/// Plane is XY
let CanonicalPlaneSpace = Vector3(0.0f,0.0f,-1.0f)

type Ray =
    struct
        val Origin : Vector3
        val Direction : Vector3
        val SurfaceOrigin : uint64

        new(origin, dir) = { Origin = origin; Direction = NormalizedOrFail(dir);SurfaceOrigin = 0UL }
        new(origin, dir, id) = { Origin = origin; Direction = NormalizedOrFail(dir);SurfaceOrigin = id }

    end

[<AbstractClass>]
type Hitable ()  =
    abstract member HasIntersection: Ray -> bool
    abstract member Intersect: Ray -> bool*LineParameter
    abstract member IntersectionAcceptable : bool -> LineParameter -> float32 -> Point -> bool
    abstract member NormalForSurfacePoint : Point -> Normal
    abstract member IsObstructedBySelf: Ray -> bool 
    abstract member TMin : float32
    abstract member TMax : float32

    // effects shadow acne
    default this.TMin = 0.001f
    default this.TMax = 500.0f
    default this.HasIntersection _ = false
    default this.Intersect _ = (false, 0.0f)
    default this.IntersectionAcceptable _ _ _ _ = false
    default this.NormalForSurfacePoint _ = Vector3.Zero
    default this.IsObstructedBySelf _ = false


type NotHitable() = inherit Hitable ()

type Sphere(sphereCenter : Origin,radius : Radius) =
    inherit Hitable () with

        // override this.TMin = 0.000001f// 0.0001// 0.000001f
        member this.Center = sphereCenter
        member this.Radius = radius
        member this.GetIntersections (_,i1,i2) = (i1,i2)
        member this.IntersectWith (t : LineParameter) (ray : Ray) =
            ((ray.Origin + t*ray.Direction) - this.Center).Length() <= this.Radius

        member this.Intersections (ray : Ray) = 
            //A is always one
            let centerToRay = ray.Origin - this.Center
            let B = 2.0f*Vector3.Dot(centerToRay,ray.Direction)
            let C = Vector3.Dot(centerToRay,centerToRay)-this.Radius**2.0f
            let discriminant = B**2.0f - 4.0f*C
            if discriminant < 0.0f then (false, 0.0f,0.0f)
            // TODO: may cause alsiasing investigate around sphere edges
            else if Round discriminant 3 = 0.0f then (false,-B/(2.0f),System.Single.MinValue)
            else (true,((-B + MathF.Sqrt(discriminant))/(2.0f)),((-B - MathF.Sqrt(discriminant))/(2.0f)))

        override this.Intersect (ray : Ray) = 
            let (hasIntersection,i1,i2) = this.Intersections (ray : Ray)
            if i1 >= this.TMin && i2 >= this.TMin then
                (hasIntersection,MathF.Min(i1,i2))
            else if i1 < 0.0f then
                (hasIntersection,i2)
            else
                (hasIntersection,i1)
        override this.NormalForSurfacePoint (positionOnSphere:Point) =
            Vector3.Normalize((positionOnSphere - this.Center))
        override this.HasIntersection ray =
            let (hasIntersection,_,_) = this.Intersections ray 
            hasIntersection
        override this.IntersectionAcceptable hasIntersection t _ _ =
            hasIntersection && t > this.TMin
        override this.IsObstructedBySelf ray =
            let (b,i1,i2) = this.Intersections ray
            this.IntersectionAcceptable b (MathF.Max(i1,i2)) 1.0f Vector3.Zero
 

//TODO enforce boundaries so that it is no longer infinite
type Plane(plane : System.Numerics.Plane, center : Point option, width : float32 option, height : float32 option ) = 
    inherit Hitable () with
        member this.Plane = plane
        member this.Normal = this.Plane.Normal
        member this.Center = center
        member this.Width = width
        member this.Height = height
        member this.PointLiesInRectangle (point : Point) =
            let widthOff = this.Width.Value / 2.0f
            let heightOff = this.Height.Value / 2.0f
            let R = RotationBetweenUnitVectors this.Normal CanonicalPlaneSpace
            let kern = if this.Plane.D > 0.0f then -1.0f*this.Plane.D*this.Normal else this.Plane.D*this.Normal
            let v = point - kern
            let b = this.Center.Value - kern
            let newDir = Vector4.Transform((ToHomogeneous v 0.0f), R)
            let newDir_b = Vector4.Transform((ToHomogeneous b 0.0f), R)
            let newP = kern + (ToVec3 newDir)
            let newB = kern + (ToVec3 newDir_b)
            newP.X <= newB.X + widthOff && 
            newP.X >= newB.X - widthOff && 
            newP.Y <= newB.Y + heightOff && 
            newP.Y >= newB.Y - heightOff

        override this.Intersect (ray:Ray) =
            let numerator = -this.Plane.D - Plane.DotNormal(this.Plane,ray.Origin) 
            let denominator = Plane.DotNormal(this.Plane,ray.Direction)
            if Math.Abs(denominator) < this.TMin then (false, 0.0f)
            else (true, numerator / denominator)
        override this.HasIntersection (ray:Ray) = 
            let (hasIntersection,_) = this.Intersect ray 
            hasIntersection
        // dotView factor ensures sampling "straight" at very large distances due to fov
        override this.IntersectionAcceptable hasIntersection t dotViewTrace pointOnSurface =
            let generalIntersection = hasIntersection && t > this.TMin && t <= (this.TMax/dotViewTrace)
            match this.Center with
                | Some _ -> generalIntersection && this.PointLiesInRectangle pointOnSurface
                | None -> generalIntersection
        override this.NormalForSurfacePoint _ =
            this.Normal
        override this.IsObstructedBySelf _ = false


/// Does the ray penetrating the surface have a t < tCompare
let IsIntersectionInfrontOf (geometry : Hitable) (ray : Ray) (tCompare : LineParameter) = 
        let (hasIntersections,t) = geometry.Intersect ray
        if hasIntersections && t > geometry.TMin then t < tCompare else false

let ParameterToPointForRay (ray : Ray) (point : Point) =
    if ray.Direction.X = 0.0f then (point.Y - ray.Origin.Y)/ray.Direction.Y
    else (point.X - ray.Origin.X)/ray.Direction.X

let PointForRay (ray : Ray) (t : LineParameter) = ray.Origin + t*ray.Direction

// let DoesRayTransportLight (surfaces : Hitable list) (ray : Ray) (light: Hitable) = 
//     // let t_HitLight = ParameterToPointForRay ray lightWS
//     let (b,t_HitLight) = light.Intersect ray
//     if light.IntersectionAcceptable b t_HitLight 1.0f then 
//         let intersections = 
//             seq { for surface in surfaces do yield (not (IsIntersectionInfrontOf surface ray t_HitLight))}
//         Seq.fold (fun b1 b2 -> b1 && b2) true intersections
//     else false

let smallestIntersection (b,t,x) (b_new,t_new,x_new) =
    if t <= t_new then (b,t,x)
    else (b_new,t_new,x_new)

let flattenIntersection ((b,t),x) = (b,t,x)
       

