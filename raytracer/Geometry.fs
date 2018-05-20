module Raytracer.Geometry

open System
open System.Numerics
open Raytracer.Numerics
open SixLabors.ImageSharp

type Origin = Vector3 // Position of a point in 3D space
type Direction = Vector3  
type Normal = Vector3
type Offset = float32  
type Radius = float32
type LineParameter = float32
type Point = Vector3
type ID = uint64

type Ray =
    struct
        val Origin : Vector3
        val Direction : Vector3

        new(origin, dir) = { Origin = origin; Direction = normalized(dir) }

    end

type Hitable() =
    abstract member HasIntersection: Ray -> bool
    abstract member Intersect: Ray -> bool*LineParameter
    abstract member IntersectionAcceptable : bool -> LineParameter -> float32  -> bool
    abstract member NormalForSurfacePoint : Point -> Normal
    abstract member IsRayObstructed: Hitable list -> Ray -> bool
    abstract member Color: Vector4
    abstract member ID: ID


    static member ToHitable x = x:> Hitable 
    member this.TMin = 0.01f
    member this.TMax = 50.0f



    default this.HasIntersection _ = false
    default this.Intersect _ = (false,0.0f)
    default this.IntersectionAcceptable _ _ _ = false
    default this.NormalForSurfacePoint _ = Vector3.Zero
    default this.IsRayObstructed _ _ = false
    default this.Color = Rgba32.White.ToVector4()
    default this.ID = (uint64)0

type Sphere(sphereCenter : Origin,radius : Radius, id: ID) =
    inherit Hitable()
    override this.ID = id
    member this.Center = sphereCenter
    member this.Radius = radius
    override this.Color = Rgba32.RoyalBlue.ToVector4()
    member this.GetIntersections (_,i1,i2) = (i1,i2)
    member this.Intersections (ray : Ray) = 
        let centerToRay = ray.Origin - this.Center
        let dirDotCenterToRay = Vector3.Dot(ray.Direction ,centerToRay)
        let discriminant = 
            MathF.Pow(dirDotCenterToRay, 2.0f) - centerToRay.LengthSquared() + this.Radius**2.0f
        if discriminant < 0.0f then (false, 0.0f,0.0f)
        else if round discriminant 1 = 0.0f then (true,-dirDotCenterToRay,0.0f)
        else (true,-dirDotCenterToRay + MathF.Sqrt(discriminant),-dirDotCenterToRay - MathF.Sqrt(discriminant))
    override this.Intersect (ray : Ray) = 
        let (hasIntersection,i1,i2) = this.Intersections (ray : Ray)
        (hasIntersection,smallestNonNegative (i1,i2))
    member this.IntersectWith (t : LineParameter) (ray : Ray) =
        ((ray.Origin + t*ray.Direction) - this.Center).Length() <= this.Radius
    override this.NormalForSurfacePoint (positionOnSphere:Point) =
        Vector3.Normalize(positionOnSphere - this.Center)
    override this.HasIntersection ray =
        let (hasIntersection,_,_) = this.Intersections ray 
        hasIntersection
    override this.IntersectionAcceptable hasIntersection t _ =
        hasIntersection && t > this.TMin
    //TODO Implement Is Ray obstructed for spheres


type Plane(plane : System.Numerics.Plane, id : ID) = 
    inherit Hitable()
    override this.ID = id
    member this.Plane = plane
    member this.Normal = this.Plane.Normal
    override this.Color = Rgba32.Snow.ToVector4()
    override this.Intersect (ray:Ray) =
        let numerator = -this.Plane.D - Plane.DotNormal(this.Plane,ray.Origin) 
        let denominator = Plane.DotNormal(this.Plane,ray.Direction)
        if Math.Abs(round denominator 2) < 0.01f  then (false, 0.0f)
        else (true, numerator / denominator)
    override this.HasIntersection (ray:Ray) = 
        let (hasIntersection,_) = this.Intersect ray 
        hasIntersection
    override this.IntersectionAcceptable hasIntersection t dotViewAndTracingRay =
        hasIntersection && t > this.TMin && t <= (this.TMax/dotViewAndTracingRay)
    override this.NormalForSurfacePoint _ =
        this.Normal
    override this.IsRayObstructed surfaces ray = 
        let intersections = 
            seq { for surface in surfaces do yield surface.HasIntersection ray}
        Seq.fold (fun b1 b2 -> b1 || b2) false intersections


let isRayObstructed (spheres : Sphere list ) (ray : Ray) =
    let intersections = 
        seq { for sphere in spheres do yield sphere.HasIntersection ray}
    Seq.fold (fun b1 b2 -> b1 || b2) false intersections