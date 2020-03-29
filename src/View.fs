module View

open Feliz
open App
open Model
open Helpers

let cellClass = function
    | Unopened | Flagged -> "has-background-grey-lighter"
    | Empty | HasAdjacentMines _ -> "has-background-white-bis"
    | Mine -> "is-danger"

let cellIconClass = function
    | Unopened | Empty | HasAdjacentMines _ -> ""
    | Flagged -> "fas fa-exclamation"
    | Mine -> "fas fa-skull-crossbones"

let renderCell isGameOver dispatch cell =
    Html.button [
        prop.classes [ "button"; cellClass cell.state ]
        prop.style [ style.margin 1; style.height 30; style.width 30 ]
        prop.onContextMenu (fun ev -> ev.preventDefault(); ToggleFlag (cell.point, cell.state) |> dispatch)
        prop.onClick (fun _ -> OpenCell (cell.point, cell.state) |> dispatch)
        prop.disabled isGameOver
        match cell.state with
        | HasAdjacentMines count ->
            prop.text (string count)
        | _ ->
            prop.children [
                Html.i [ prop.className (cellIconClass cell.state) ]
            ]
    ]

let renderCellRow isGameOver dispatch rowCells =
    let cells = rowCells |> List.sortBy (fun c -> c.point.x)
    Html.div [
        prop.className "columns is-paddingless is-marginless"
        prop.children (cells |> List.map (renderCell isGameOver dispatch))
    ]

let renderGrid state dispatch =
    let getCellRow i = List.where (fun c -> c.point.y = i) 
    Html.div [
        prop.children [
            for y in 0 .. (state.currentSettings.height - 1) do
                state.cells |> getCellRow y |> renderCellRow (isGameOver state.status) dispatch
        ]
    ]

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let renderHero state =
    Html.section [
        prop.className "hero is-dark"
        prop.children [
            div [ "hero-head" ] [
                Html.nav [
                    prop.className "navbar"
                    prop.children [
                        div [ "navbar-menu" ] [
                            div [ "navbar-end" ] [
                                Html.span [
                                    prop.className "navbar-item"
                                    prop.children [
                                        Html.a [
                                            prop.className "button is-info is-inverted"
                                            prop.href "https://github.com/ssiltanen/MineSweeper"
                                            prop.children [
                                                Html.span [
                                                    prop.className "icon"
                                                    prop.children [
                                                        Html.i [ prop.className "fab fa-github" ]
                                                    ]
                                                ]
                                                Html.span [ Html.p "Github" ]
                                            ]
                                        ]  
                                    ]
                                ]
                            ]
                        ]  
                    ]
                ]
            ]
            Html.div [ 
                prop.className "hero-body"
                prop.style [ style.paddingTop 0 ]
                prop.children [
                    Html.div [
                        Html.h1 [ 
                            prop.className "title is-2"
                            prop.text "Mine Sweeper"
                        ]
                        Html.h1 [ 
                            prop.className "subtitle"
                            prop.text (sprintf "%i Mines left" (state.currentSettings.mines - countFlaggedCells state))
                        ]
                    ]
                ]
            ]
        ]
    ]


let renderGameLost =
    Html.p [ 
        prop.className "subtitle has-text-danger"
        prop.style [ style.textAlign.center ]
        prop.text "Game over"
    ]

let renderGameWon =
    Html.p [ 
        prop.className "subtitle has-text-success"
        prop.style [ style.textAlign.center ]
        prop.text "You won! Congrats!"
    ]

let renderLabel name =
    Html.label [
        prop.className "label"
        prop.for' name
        prop.text name
    ]

let renderInput name (value : int) (maxValue : int) (maxLength : int) event dispatch =
    Html.input [
        prop.className "input"
        prop.name name
        prop.defaultValue value
        prop.maxLength maxLength
        prop.min 5
        prop.max maxValue
        prop.onChange (tryParseAboveZeroInt >> Option.iter (event >> dispatch))
    ]

let renderNewGameForm initializing settings dispatch =
    Html.div [
        prop.className "column is-narrow"
        prop.children [
            renderLabel "Width"
            renderInput "Width" settings.width 50 2 SetWidth dispatch
            renderLabel "Height"
            renderInput "Height" settings.height 50 2 SetHeight dispatch
            renderLabel "Mines"
            renderInput "Mines" settings.mines (settings.width * settings.height) 3 SetMinesCount dispatch
            Html.button [
                prop.className "button is-primary is-light"
                prop.style [ style.marginTop 10; style.marginBottom 10; style.width 100 ]
                prop.type'.submit
                prop.disabled (invalidSettings settings || initializing)
                prop.onClick (fun _ -> dispatch NewGame)
                prop.children [
                    if initializing then Html.i [ prop.className "fa fa-cog fa-spin fa-2x" ]
                    else Html.p "New Game"
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.children [
            Html.div [
                renderHero state
                Html.div [
                    prop.className "columns is-mobile is-paddingless is-marginless"
                    prop.children [
                        Html.div [ 
                            prop.className "column is-narrow"
                            prop.style [ style.backgroundColor.papayaWhip ]
                            prop.children [ 
                                renderNewGameForm (isInitializing state.status) state.inputSettings dispatch
                                match state.status with
                                | Won -> renderGameWon
                                | Lost -> renderGameLost
                                | InProgress | Initializing -> Html.none ]
                        ]
                        Html.div [
                            prop.className "column"
                            prop.style [ ]
                            prop.children [ renderGrid state dispatch ]
                        ]
                    ]
                ]
            ]
        ]
    ]
