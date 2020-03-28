module App

open System
open Feliz
open Elmish

type RenderState = Unknown | Empty | HasAdjacentMines of int | Flagged | Mine
type Point = { x : int; y : int } with
    static member Create (x, y) = { x = x; y = y }
type Cell = 
    { point : Point; state : RenderState; isMined : bool } with
    static member Create point = { point = point; state = Unknown; isMined = false }

type State = 
    { width : int
      height : int
      mines : int
      cells : Cell list }

type Msg =
    | ToggleFlag of Point * RenderState
    | Open of Point * RenderState

let rand = Random()

let assignMines count cells =
    let setMine cell = { cell with isMined = true }
    let cells = cells |> List.sortBy (fun _ -> rand.Next(0,1000)) |> List.sortBy (fun _ -> rand.Next(0,1000))
    let mined = cells |> List.take count |> List.map setMine
    mined @ (cells |> List.skip count) |> List.sortBy (fun c -> c.point.y, c.point.x)

let init() =
    let width = 10
    let height = 10
    let minesCount = 20
    let axisPoints count = [ 0 .. (count - 1) ]
    let cells = 
        List.allPairs (axisPoints width) (axisPoints height) 
        |> List.map (Point.Create >> Cell.Create) 
        |> assignMines minesCount
    { width = width; height = height; mines = minesCount; cells = cells}, Cmd.none

let updateCell point (f : Cell -> Cell) =
    List.map (fun c -> if c.point = point then f c else c)

let flagCell point state =
    let flag = fun c -> { c with state = Flagged }
    { state with cells = state.cells |> updateCell point flag }

let unflagCell point state =
    let unflag = fun c -> { c with state = Unknown }
    { state with cells = state.cells |> updateCell point unflag }

let getAdjacentCells point state =
    List.allPairs [-1; 0; 1] [-1; 0; 1]
    |> List.where (fun (x, y) -> x <> 0 || y <> 0)
    |> List.map (
        fun (x,y) -> Point.Create(point.x + x, point.y + y)
        >> fun p -> state.cells |> List.tryFind (fun c -> c.point = p))
    |> List.choose id

let countAdjacentMines point state =
    getAdjacentCells point state
    |> List.where (fun c -> c.isMined)
    |> List.length

let openCell point state =
    let openCell = fun c ->
        let newState =
            if c.isMined then Mine
            else 
                let adjacentMinesCount = countAdjacentMines point state
                if adjacentMinesCount = 0 then Empty
                else HasAdjacentMines adjacentMinesCount 
        { c with state = newState }
    { state with cells = state.cells |> updateCell point openCell }

let update (msg: Msg) (state: State) =
    match msg with
    | ToggleFlag (point, Unknown) -> flagCell point state, Cmd.none
    | ToggleFlag (point, Flagged) -> unflagCell point state, Cmd.none
    | ToggleFlag _ -> state, Cmd.none
    | Open (point, Unknown) -> openCell point state, Cmd.none
    | Open _ -> state, Cmd.none

let cellClass = function
    | Unknown | Flagged -> "has-background-grey-lighter"
    | Empty | HasAdjacentMines _ -> "has-background-white-bis"
    | Mine -> "is-danger"

let cellIconClass = function
    | Unknown | Empty | HasAdjacentMines _ -> ""
    | Flagged -> "fas fa-exclamation"
    | Mine -> "fas fa-skull-crossbones"

let renderCell dispatch cell =
    Html.button [
        prop.classes [ "button"; cellClass cell.state ]
        prop.style [ style.margin 1; style.height 30; style.width 30]
        prop.onContextMenu (fun ev -> ev.preventDefault(); ToggleFlag (cell.point, cell.state) |> dispatch)
        prop.onClick (fun _ -> Open (cell.point, cell.state) |> dispatch)
        match cell.state with
        | HasAdjacentMines count ->
            prop.text (string count)
        | _ ->
            prop.children [
                Html.i [ prop.className (cellIconClass cell.state) ]
            ]
    ]

let renderCellRow dispatch rowCells =
    let cells = rowCells |> List.sortBy (fun c -> c.point.x)
    Html.div [
        prop.classes [ "columns"; "is-paddingless"; "is-marginless" ]
        prop.children (cells |> List.map (renderCell dispatch))
    ]

let renderGrid state dispatch =
    let getCellRow i = List.where (fun c -> c.point.y = i) 
    Html.div [
        prop.classes [ "columns"; "is-mobile" ]
        prop.children [
            Html.div [ prop.classes [ "column" ] ]
            Html.div [
                prop.classes [ "column"; "is-narrow" ]
                prop.children [
                    for y in 0 .. (state.height - 1) do
                        state.cells |> getCellRow y |> renderCellRow dispatch
                ]
            ]
            Html.div [ prop.classes [ "column" ] ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.children [
            Html.p [ 
                prop.className "title"
                prop.style [ style.textAlign.center ]
                prop.text "Mine Sweeper"
            ]
            renderGrid state dispatch
        ]
    ]
