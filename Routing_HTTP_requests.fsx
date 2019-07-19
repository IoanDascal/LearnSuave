#r "C:/Users/Nelu/Desktop/fsvsc/packages/packages/Suave/lib/net40/suave.dll"
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let app=
    choose
        [
            GET >=> choose
                        [
                            path "/hello" >=> OK "Hello GET"
                            path "/goodbye" >=> OK "goodbye GET"
                        ]
            POST >=> choose
                        [
                            path "/hello" >=> OK "Hello POST"
                            path "/goodbye" >=> OK "Goodbye POST"
                        ]
        ]

startWebServer defaultConfig app