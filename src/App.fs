module App

open System
open Feliz
open Elmish

type RenderState = Unopened | Empty | HasAdjacentMines of int | Flagged | Mine
type Point = { x : int; y : int } with
    static member Create (x, y) = { x = x; y = y }
type Cell = 
    { point : Point; state : RenderState; isMined : bool } with
    static member Create point = { point = point; state = Unopened; isMined = false }

type State = 
    { width : int
      height : int
      mines : int
      cells : Cell list
      isGameOver : bool }

type Msg =
    | ToggleFlag of Point * RenderState
    | OpenCell of Point * RenderState
    | GameOver

let width = 10
let height = 10
let minesCount = 10

let rand = Random()

let assignMines count cells =
    let setMine cell = { cell with isMined = true }
    let cells = cells |> List.sortBy (fun _ -> rand.Next(0,1000)) |> List.sortBy (fun _ -> rand.Next(0,1000))
    let mined = cells |> List.take count |> List.map setMine
    mined @ (cells |> List.skip count) |> List.sortBy (fun c -> c.point.y, c.point.x)

let init() =
    let axisPoints count = [ 0 .. (count - 1) ]
    let cells = 
        List.allPairs (axisPoints width) (axisPoints height) 
        |> List.map (Point.Create >> Cell.Create) 
        |> assignMines minesCount
    { width = width; height = height; mines = minesCount; cells = cells; isGameOver = false }, Cmd.none

let updateCell point (f : Cell -> Cell) =
    List.map (fun c -> if c.point = point then f c else c)

let flagCell state point =
    let flag = fun c -> { c with state = Flagged }
    { state with cells = state.cells |> updateCell point flag }

let unflagCell state point =
    let unflag = fun c -> { c with state = Unopened }
    { state with cells = state.cells |> updateCell point unflag }

let adjacentCells state point =
    List.allPairs [-1; 0; 1] [-1; 0; 1]
    |> List.where (fun (x, y) -> x <> 0 || y <> 0)
    |> List.map (
        fun (x,y) -> Point.Create(point.x + x, point.y + y)
        >> fun p -> state.cells |> List.tryFind (fun c -> c.point = p))
    |> List.choose id

let countAdjacentMines state point  =
    adjacentCells state point
    |> List.where (fun c -> c.isMined)
    |> List.length

let openCell state point =
    let openCell cell =
        let newState =
            if cell.isMined then Mine
            else 
                let adjacentMinesCount = countAdjacentMines state point
                if adjacentMinesCount = 0 then Empty
                else HasAdjacentMines adjacentMinesCount 
        { cell with state = newState }
    { state with cells = state.cells |> updateCell point openCell }

let openCellAndEmptyAdjacentCells state point =
    if state.cells |> List.exists (fun cell -> cell.point = point && cell.isMined)
    then openCell state point, Cmd.ofMsg GameOver
    else
        let cellIsUnopened cell = match cell.state with | Unopened -> true | _ -> false
        let rec openAdjacentEmptyCells point state =
            adjacentCells state point
            |> List.where cellIsUnopened
            |> List.fold (fun state cell ->
                let count = countAdjacentMines state cell.point
                if count > 0 then openCell state cell.point
                else openCell state cell.point |> openAdjacentEmptyCells cell.point) state

        let state = openCell state point
        match state.cells |> List.find (fun cell -> cell.point = point) |> fun cell -> cell.state with
        | Empty -> openAdjacentEmptyCells point state, Cmd.none
        | HasAdjacentMines _ -> state, Cmd.none
        | state -> failwithf "Error: Opened cell had invalid state %A" state

let update (msg: Msg) (state: State) =
    match msg with
    | ToggleFlag (point, Unopened) -> flagCell state point, Cmd.none
    | ToggleFlag (point, Flagged) -> unflagCell state point, Cmd.none
    | ToggleFlag _ -> state, Cmd.none
    | OpenCell (point, Unopened) -> openCellAndEmptyAdjacentCells state point
    | OpenCell _ -> state, Cmd.none
    | GameOver -> { state with isGameOver = true }, Cmd.none

let cellClass = function
    | Unopened | Flagged -> "has-background-grey-lighter"
    | Empty | HasAdjacentMines _ -> "has-background-white-bis"
    | Mine -> "is-danger"

let cellIconClass = function
    | Unopened | Empty | HasAdjacentMines _ -> ""
    | Flagged -> "fas fa-exclamation"
    | Mine -> "fas fa-skull-crossbones"

let renderCell dispatch cell =
    Html.button [
        prop.classes [ "button"; cellClass cell.state ]
        prop.style [ style.margin 1; style.height 30; style.width 30]
        prop.onContextMenu (fun ev -> ev.preventDefault(); ToggleFlag (cell.point, cell.state) |> dispatch)
        prop.onClick (fun _ -> OpenCell (cell.point, cell.state) |> dispatch)
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
