module Raytracer.Material

open System.Numerics
open Raytracer.Numerics
open SixLabors.ImageSharp

type Color = Vector3

[<Struct>]
type Material =
        val Color : Color
        new (color : Color) = {Color = color}
        new (color : Rgba32) = {Color = (ToVec3 (color.ToVector4()))}
