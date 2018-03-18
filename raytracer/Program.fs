// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open System.IO

let createImage = 
    using (File.OpenWrite("test.jpg")) (fun output ->
        using(new Image<Rgba32>(640, 480))(fun image -> 
            for px in 200..300 do
                for py in 200..300 do
                    image.Item(px,py) <- Rgba32.White
            image.SaveAsJpeg(output)
        )
    )  

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    createImage
    0 // return an integer exit code




    
