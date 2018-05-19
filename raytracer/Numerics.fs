module Raytracer.Numerics

open System.Numerics
open System

let unitParameter (value : float32) = 
    if value >= 0.0f && value <= 1.0f then value else failwith "parameter not in range [0,1]"

let round (num :float32) (digits:int) = MathF.Round(num,digits)

//TODO: Investigate Vector3 normalization in fsharp
let normalized(value : Vector3) = 
    if round (value.Length()) 5 = 1.0f then value else failwith "Vector3 not normalized"

let roundVec3 (vec3:Vector3) (digits:int) =
    Vector3(round vec3.X digits, round vec3.Y digits, round vec3.Z digits)

let smallestNonNegative (a:float32,b:float32) =
    if a < 0.0f then b
    else if b < 0.0f then a
    else if a <= b then a
    else b

let toVec3 (vec4:Vector4) =
    Vector3(vec4.X,vec4.Y,vec4.Z)

let lightTransportWithObstacle b =
    match b with
        | true -> 0.0f
        | false -> 1.0f
        



    





