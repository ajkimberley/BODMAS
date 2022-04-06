module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open BODMAS.Server.Parser
open Saturn

open Shared

let bodmasApi =
    { processInput = fun (input: string) -> async { return parse input } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue bodmasApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
