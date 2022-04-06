module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Input: string; ProcessedInput: Result<Node,string> option }

type Msg =
    | ProcessInput
    | ProcessedInput of Result<Node,string>
    | SetInput of string

let bodmasApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBodmasApi>

let init () : Model * Cmd<Msg> =
    let model = { Input = ""; ProcessedInput = Option.None }
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ProcessInput -> 
        let cmd = Cmd.OfAsync.perform bodmasApi.processInput model.Input ProcessedInput
        { model with Input = "" }, cmd
    | ProcessedInput result -> { model with ProcessedInput = Some result }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let rec processBinaryNode (node: BinaryOperatorNode) =
    Html.li [
        prop.children [
            Html.span node.Token
            Html.ul [
                processNode node.LHS
                processNode node.RHS
            ]
        ]
    ]

and processNode (node: Node) : ReactElement =
    match node with
    | LeafNode node -> Html.li node.Token
    | BinaryOperatorNode node -> processBinaryNode node

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        match model.ProcessedInput with
        | Some (Ok node) ->
            Bulma.content [
                prop.style [
                    style.marginLeft length.auto
                    style.marginRight length.auto
                ]
                prop.children[
                    Html.ul [
                        prop.className "tree"
                        prop.children [
                            processNode node
                        ]
                    ]
                ]
            ]
        | _ -> Html.div "No Content"
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "Enter an arithmetic expression."
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.onClick (fun _ -> dispatch ProcessInput)
                        prop.text "Parse"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundPosition "no-repeat center center fixed"
        ]
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
                                prop.text "BODMAS"
                            ]
                            Bulma.subtitle [
                                text.hasTextCentered
                                prop.text "(Brackets, Ordinals, Division, Multipllication, Addition, Subtraction)"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
