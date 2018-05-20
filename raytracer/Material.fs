module Raytracer.Material

open System.Numerics

type Color = Vector4

type Material(color : Color) =
    member this.Color = color

let AttenuationForRay isObstructed =
    match isObstructed with
        | true -> 0.0f
        | false -> 1.0f