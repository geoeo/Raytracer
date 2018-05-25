module Raytracer.Numerics

open System.Numerics
open System

let UnitParameter (value : float32) = 
    if value >= 0.0f && value <= 1.0f then value else failwith "parameter not in range [0,1]"

let Round (num :float32) (digits:int) = MathF.Round(num,digits)

//TODO: Investigate Vector3 normalization in fsharp
let Normalized(value : Vector3) = 
    if Round (value.Length()) 5 = 1.0f then value else failwith "Vector3 not normalized"

let RoundVec3 (vec3:Vector3) (digits:int) =
    Vector3(Round vec3.X digits, Round vec3.Y digits, Round vec3.Z digits)

let ToVec3 (vec4:Vector4) =
    Vector3(vec4.X,vec4.Y,vec4.Z)

let ApplyFuncToVector3 func (vec : Vector3) = Vector3(func vec.X,func vec.Y,func vec.Z)

let Power exp b = MathF.Pow(b,exp)

let Rotation (matrix : Matrix4x4)
    = Matrix4x4(matrix.M11,matrix.M12,matrix.M13,0.0f,
                matrix.M21,matrix.M22,matrix.M23,0.0f,
                matrix.M31,matrix.M32,matrix.M33,0.0f,
                0.0f,0.0f,0.0f,0.0f)

let TransposeRot (matrix : Matrix4x4) =
        Matrix4x4(matrix.M11,matrix.M21,matrix.M31,0.0f,
              matrix.M12,matrix.M22,matrix.M32,0.0f,
              matrix.M13,matrix.M23,matrix.M33,0.0f,
              0.0f,0.0f,0.0f,0.0f)

let SkewSymmetrix (v : Vector3) =
    Matrix4x4

/// Computes the SO3 Matrix from a to b
/// https://math.stackexchange.com/questions/180418/calculate-rotation-matrix-to-align-vector-a-to-vector-b-in-3d/897677#897677
// let RotationBetweenUnitVectors (a : Vector3) (b : Vector3) =
//    let v1 = Normalized(a)
//    let v2 = Normalized(b)




        



    





