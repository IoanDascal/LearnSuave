//https://suave.io/composing.html
#r "C:/Users/Nelu/Desktop/fsvsc/packages/packages/Suave/lib/net40/suave.dll"
open Suave
open Suave.Successful
open Suave.Utils
open Suave.Filters
open Suave.Operators

let optionFromChoice ch=
    match ch with
    | Choice1Of2 a -> Some a
    | Choice2Of2 _ -> None
    
let greetings q=
    defaultArg   (optionFromChoice (q ^^ "color"))  "World" |> sprintf "Hello %s"

let sample : WebPart=
    path "/hello" >=> choose [
        GET >=> request (fun r -> OK <| greetings r.query)
        //GET >=> request (fun r -> printfn "GRG=%A"  r
          //                        OK (greetings r.query))
        POST >=> request (fun r -> printfn "GRP=%A"  r
                                   OK (greetings r.form))
        RequestErrors.NOT_FOUND "Found no handlers"
    ]

startWebServer defaultConfig sample