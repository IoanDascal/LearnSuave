#r "C:/Users/Nelu/Desktop/fsvsc/packages/packages/Suave/lib/net40/suave.dll"
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let sleep milliseconds message: WebPart=
    fun (x : HttpContext) -> 
        async{
            do! Async.Sleep milliseconds
            return! OK message x
        }
let app=
    choose
        [
            GET >=> choose
                        [
                            path "/hello" >=> sleep 5000 "Hello guys"
                        ]
        ]

startWebServer defaultConfig app