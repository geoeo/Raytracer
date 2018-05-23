module Raytracer.Material

open System.Numerics

type Color = Vector3

[<Struct>]
type Material(color : Color) =
        member this.Color = color
