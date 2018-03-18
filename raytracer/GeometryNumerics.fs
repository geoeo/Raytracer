module GeometryNumerics

open System.Numerics

let normalized(value : Vector3) = 
    if value.Length() = 1.0f then value else failwith "Vector3 not normalized"

let unitParameter (value : float32) = 
    if value >= 0.0f && value <= 1.0f then value else failwith "parameter not in range [0,1]"