module Index

open System.Text.RegularExpressions

open Elmish
open Fable.Remoting.Client
open Feliz
open Feliz.Bulma

open Shared


type Model =
    { Input: string
      Results: Result list }

type Msg =
    | SetInput of string
    | Calculate of BinOp * float * float
    | GetResult of Result

let calcApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<calculator>

let init () : Model * Cmd<Msg> =
    { Input = ""; Results = [] }, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetInput str ->
        { model with Input = str }, Cmd.none
    | Calculate(op, lhs, rhs) ->
        let operands = { lhs = lhs; rhs = rhs }
        match op with
        | Add ->
            model, Cmd.OfAsync.perform calcApi.addition operands GetResult
        | Subtract ->
            model, Cmd.OfAsync.perform calcApi.subtraction operands GetResult
        | Multiply ->
            model, Cmd.OfAsync.perform calcApi.multiplication operands GetResult
        | Divide ->
            model, Cmd.OfAsync.perform calcApi.division operands GetResult
    | GetResult r ->
        if List.contains r model.Results then
            model, Cmd.none
        else
            { model with Results = List.append model.Results [ r ] }, Cmd.none


let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    let serialize r =
        let op =
            match r.operation with
            | Add -> "+"
            | Subtract -> "-"
            | Multiply -> "*"
            | Divide -> "/"
        $"{r.lhs} {op} {r.rhs} = {r.result}"

    let tryParse (s: string) =
        try
            let s = s.Trim()
            let [| lhs; op; rhs |] = Regex.Split(s, @"\s+")
            let lhs = float lhs
            let rhs = float rhs
            let op =
                match char op with
                | '+' -> Add
                | '-' -> Subtract
                | '*' -> Multiply
                | '/' -> Divide
            Some(op, lhs, rhs)
        with
        | :? System.Exception -> None

    let expr = tryParse model.Input

    Bulma.box [
        Bulma.content [
            Html.ol [
                for r in model.Results do
                    Html.ul [ prop.text (serialize r) ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.onChange (SetInput >> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        if Option.isSome expr then color.isPrimary else color.isWarning
                        prop.disabled (Option.isNone expr)
                        prop.onClick (fun _ -> Option.get expr |> Calculate |> dispatch)
                        prop.text "="
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Web Calculator"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
