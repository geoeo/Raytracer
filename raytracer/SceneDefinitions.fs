module Raytracer.SceneDefinitions

open System.Numerics
open SixLabors.ImageSharp
open Raytracer.Surface
open Raytracer.Geometry
open Raytracer.Material
open Raytracer.Numerics

let mutable id : ID = 1UL


let assignIDAndIncrement idIn : ID =
    let toBeAssigned = idIn
    id <- id + 1UL
    toBeAssigned

let lights : Surface list 
    =  [
        //Emitting(assignIDAndIncrement id, Sphere(Vector3(3.0f, 8.0f, -5.0f),3.0f), Material(Vector3(1.0f,1.0f,1.0f)));
        // Emitting(assignIDAndIncrement id, Sphere(Vector3(-4.0f, 8.0f, -10.0f),3.0f), Material(Vector3(1.0f,1.0f,1.0f)));
        NoSurface(assignIDAndIncrement id, Plane(new System.Numerics.Plane((SurfaceNormal -1.0f -1.0f 1.0f),20.0f),Some ((SurfaceNormal -1.0f -1.0f 1.0f)*(-20.0f)),Some 40.0f,Some 13.0f), Material(Rgba32.White, 0.0f, Rgba32.White, 1.0f));
        NoSurface(assignIDAndIncrement id, Plane(new System.Numerics.Plane((SurfaceNormal 1.0f 0.0f 0.0f),15.0f),Some ((Vector3(-15.0f,5.0f,-10.0f))),Some 20.0f,Some 10.0f), Material(Rgba32.White,0.0f, Rgba32.White,1.0f));
    ]

let spheres : Surface list
    = [
        Dielectric(assignIDAndIncrement id,Sphere(Vector3(4.0f,-2.0f,-12.0f),2.0f), Material(Rgba32.White),1.5f);
        Metal(assignIDAndIncrement id,Sphere(Vector3(4.0f,-2.0f,-19.0f),2.0f), Material(Rgba32.RoyalBlue),0.7f);
        // Lambertian(assignIDAndIncrement id,Sphere(Vector3(2.0f,-2.0f,-14.0f),2.0f), Material(Rgba32.Green));
        Metal(assignIDAndIncrement id,Sphere(Vector3(-1.5f,-5.0f,-7.0f),1.5f), Material(Rgba32.White),0.0f);
        Lambertian(assignIDAndIncrement id,Sphere(Vector3(2.0f,-5.0f,-7.0f),1.5f), Material(Rgba32.Red));
        Lambertian(assignIDAndIncrement id,Sphere(Vector3(6.0f,-5.0f,-7.0f),1.5f), Material(Rgba32.DarkGreen));
        Dielectric(assignIDAndIncrement id,Sphere(Vector3(2.5f,-3.0f,-3.0f),1.5f), Material(Rgba32.White),1.5f);
        // Lambertian(assignIDAndIncrement id,Sphere(Vector3(-1.5f,2.5f,-9.0f),0.8f), Material(Rgba32.DarkGreen));
        // Lambertian(assignIDAndIncrement id,Sphere(Vector3(-1.5f,0.0f,-14.0f),2.0f),Material(Vector3(0.0f,0.0f,1.0f)))
        // Metal(assignIDAndIncrement id,Sphere(Vector3(-6.0f,-0.5f,-6.0f),1.0f), Material(Rgba32.White),0.0f);
        // Metal(assignIDAndIncrement id,Sphere(Vector3(-5.0f,0.0f,-21.0f),5.0f),Material(Rgba32.RoyalBlue),0.3f)
        Dielectric(assignIDAndIncrement id,Sphere(Vector3(-5.1f,2.0f,-11.0f),5.0f),Material(Rgba32.White),1.5f)
      ]
// let spheres = []

let planes : Surface list = [
    Lambertian(assignIDAndIncrement id,Plane(System.Numerics.Plane.CreateFromVertices(Vector3(-1.0f,-6.0f,0.0f),Vector3(1.0f,-6.0f,0.0f),Vector3(0.0f,-6.0f,-1.0f)),None,None,None),Material(Vector3(1.0f,1.0f,1.0f)));
    Lambertian(assignIDAndIncrement id,Plane(new System.Numerics.Plane((SurfaceNormal 0.0f 0.0f -1.0f),17.0f),Some ((Vector3(0.0f,0.0f,17.0f))),Some 30.0f,Some 10.0f), Material(Rgba32.White));
    Lambertian(assignIDAndIncrement id,Plane(new System.Numerics.Plane((SurfaceNormal 0.0f 0.0f 1.0f),30.0f),Some ((Vector3(-5.0f,10.0f,-30.0f))),Some 25.0f,Some 20.0f), Material(Rgba32.IndianRed));
    Metal(assignIDAndIncrement id, Plane(new System.Numerics.Plane((SurfaceNormal -1.0f 0.0f 1.0f),15.0f),Some ((Vector3(15.0f,3.0f,-15.0f))),Some 10.0f,Some 6.0f), Material(Rgba32.AntiqueWhite),0.05f)
    ]

let scene_spheres = List.concat [spheres;planes;lights]