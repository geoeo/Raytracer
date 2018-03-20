module Numerics

open System.Numerics
open System


let normalized(value : Vector3) = 
    if value.Length() = 1.0f then value else failwith "Vector3 not normalized"

let unitParameter (value : float32) = 
    if value >= 0.0f && value <= 1.0f then value else failwith "parameter not in range [0,1]"

let round (num :float32) (digits:int) = MathF.Round(num,digits)

// ndc is [0,1] from top left
let pixelNDC x y width height =
    (x + 0.5f)/width , (y+0.5f)/height

// screen space is [-1,1] from top left
let pixelScreen x_ndc y_ndc = 2.0f*x_ndc-1.0f , 1.0f-2.0f*y_ndc

let aspectRatio (width:float32) (height:float32) = width/height

// pixel coordiantes / sample points (X,Y) in camera space (3D)
let pixelCamera x_screen y_screen aspectRatio fov =
    x_screen*aspectRatio*MathF.Tan(fov/2.0f) , y_screen*MathF.Tan(fov/2.0f)

// ray direction wrt to camera
let rayDirection cameraPixel_x cameraPixel_y =
    Vector3.Normalize(Vector3(cameraPixel_x,cameraPixel_y,-1.0f))

let worldToCamera  position target up = Matrix4x4.CreateLookAt(position,target,up);

let cameraToWorld worldToCamera = 
    let success , cameraToWorld = Matrix4x4.Invert(worldToCamera)
    if success then cameraToWorld else failwith "Matrix4x4 Invert Failed"

// Seems to slightly less precise than Matrix4x4.Invert
// Since this is an SE3 matrix we can simply transpose R and -R_t*t
// http://ethaneade.com/lie.pdf
let cameraToWorldFast (worldToCamera : Matrix4x4) (roundToDigits: int) = 
    let rotation 
        = Matrix4x4(-worldToCamera.M11,-worldToCamera.M21,-worldToCamera.M31,0.0f,
                    -worldToCamera.M12,-worldToCamera.M22,-worldToCamera.M32,0.0f,
                    -worldToCamera.M13,-worldToCamera.M23,-worldToCamera.M33,0.0f,
                    0.0f,0.0f,0.0f,0.0f)
    let translation = Vector4(worldToCamera.M41,worldToCamera.M42,worldToCamera.M43,0.0f)
    let trans_inv = Vector4.Transform(translation,rotation)
    let round x = round x roundToDigits
    Matrix4x4(worldToCamera.M11,worldToCamera.M21,worldToCamera.M31,0.0f,
              worldToCamera.M12,worldToCamera.M22,worldToCamera.M32,0.0f,
              worldToCamera.M13,worldToCamera.M23,worldToCamera.M33,0.0f,
              round trans_inv.X,round trans_inv.Y,round trans_inv.Z,1.0f)

    





