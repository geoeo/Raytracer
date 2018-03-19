module Numerics

open System.Numerics
open System

let normalized(value : Vector3) = 
    if value.Length() = 1.0f then value else failwith "Vector3 not normalized"

let unitParameter (value : float32) = 
    if value >= 0.0f && value <= 1.0f then value else failwith "parameter not in range [0,1]"

// ndc is [0,1] from top left
let pixelNDC x y width height =
    (x + 0.5f)/width , (y+0.5f)/height

// screen space is [-1,1] from top left
let pixelScreen x_ndc y_ndc = 2.0f*x_ndc-1.0f , 1.0f-2.0f*y_ndc

let aspectRatio (width:float32) (height:float32) = width/height

// pixel coordiantes / sample points in world space
let pixelCamera x_screen y_screen aspectRatio fov =
    x_screen*aspectRatio*MathF.Tan(fov/2.0f) , y_screen*MathF.Tan(fov/2.0f)

let rayDirection cameraPixel_x cameraPixel_y =
    Vector3.Normalize(Vector3(cameraPixel_x,cameraPixel_y,-1.0f))




