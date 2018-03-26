module Raytracer.Numerics

open System.Numerics
open System

let normalized(value : Vector3) = 
    if value.Length() = 1.0f then value else failwith "Vector3 not normalized"

let unitParameter (value : float32) = 
    if value >= 0.0f && value <= 1.0f then value else failwith "parameter not in range [0,1]"

let round (num :float32) (digits:int) = MathF.Round(num,digits)

let smallestNonNegative (a:float32,b:float32) =
    if a < 0.0f then b
    else if b < 0.0f then a
    else if a <= b then a
    else b



    





