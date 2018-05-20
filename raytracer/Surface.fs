module Raytracer.Surface

open Raytracer.Geometry
open Raytracer.Material

type ID = uint64

type Surface(id: ID, geometry : Hitable, material : Material) =
    member this.ID = id
    member this.Geometry = geometry
    member this.Material = material


let AllSurfacesWithoutId (surfaces : Surface list) (id : ID) =
    List.filter (fun (surface : Surface) -> surface.ID <> id) surfaces

let SurfacesToGeometry (surfaces : Surface list) =
    List.map (fun (x : Surface) -> x.Geometry) surfaces



