module PixelConversions

open System

// ndc is [0,1] from top left
let pixelToNDC x y width height =
    (x + 0.5f)/width , (y+0.5f)/height

// screen space is [-1,1] from top left
let NDCToScreen (x_ndc , y_ndc) = 2.0f*x_ndc-1.0f , 1.0f-2.0f*y_ndc

let aspectRatio (width:float32) (height:float32) = width/height

// pixel coordiantes / sample points (X,Y) in camera space (3D)
let screenToCamera (x_screen, y_screen) aspectRatio fov =
    x_screen*aspectRatio*MathF.Tan(fov/2.0f) , y_screen*MathF.Tan(fov/2.0f)

let pixelToCamera x y width height fov =
    screenToCamera (NDCToScreen (pixelToNDC x y width height)) (aspectRatio width height) fov