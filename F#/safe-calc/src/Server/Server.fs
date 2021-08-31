module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

let calcApi =
    { addition =
          fun { lhs = lhs; rhs = rhs } ->
              async {
                  return { operation = Add
                           lhs = lhs
                           rhs = rhs
                           result = lhs + rhs }
              }
      subtraction =
          fun { lhs = lhs; rhs = rhs } ->
              async {
                  return { operation = Subtract
                           lhs = lhs
                           rhs = rhs
                           result = lhs - rhs }
              }
      multiplication =
          fun { lhs = lhs; rhs = rhs } ->
              async {
                  return { operation = Multiply
                           lhs = lhs
                           rhs = rhs
                           result = lhs * rhs }
              }
      division =
          fun { lhs = lhs; rhs = rhs } ->
              async {
                  return { operation = Divide
                           lhs = lhs
                           rhs = rhs
                           result = lhs / rhs }
              } }

let calcApiDocs =
    let docs = Docs.createFor<calculator>()
    let oneTwo = { lhs = 1.0; rhs = 2.0 }

    Remoting.documentation "Web Calculator REST API" [
        docs.route <@ fun api -> api.addition @>
        |> docs.example <@ fun api -> api.addition oneTwo @>

        docs.route <@ fun api -> api.subtraction @>
        |> docs.example <@ fun api -> api.subtraction oneTwo @>

        docs.route <@ fun api -> api.multiplication @>
        |> docs.example <@ fun api -> api.multiplication oneTwo @>

        docs.route <@ fun api -> api.division @>
        |> docs.example <@ fun api -> api.division oneTwo @>
    ]

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue calcApi
    |> Remoting.withDocs "/api/calculator/docs" calcApiDocs
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_static "public"
        use_router webApp
        use_gzip
        memory_cache
    }

run app
