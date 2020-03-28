module App

open System
open Feliz
open Elmish

type RenderState = Unopened | Empty | HasAdjacentMines of int | Flagged | Mine
type Point = { x : int; y : int } with
    static member Create (x, y) = { x = x; y = y }
type Cell = { point : Point; state : RenderState; isMined : bool } with
    static member Create point = { point = point; state = Unopened; isMined = false }

type Settings = { width : int; height : int; mines : int } with
    static member Default = { width = 30; height = 30; mines = 150 }

let maxWidth = 40
let maxHeight = 40

type GameStatus =
    | InProgress
    | Won
    | Lost

type State = 
    { inputSettings : Settings
      currentSettings : Settings
      cells : Cell list
      status : GameStatus }

type InputSetting =
    | Width of int
    | Height of int
    | Mines of int

type Msg =
    | ToggleFlag of Point * RenderState
    | OpenCell of Point * RenderState
    | GameLost
    | GameWon
    | NewGame
    | SetWidth of int
    | SetHeight of int
    | SetMinesCount of int

let rand = Random()

let tryParseAboveZeroInt (str  : string) = 
    match Int32.TryParse(str) with
    | true, i when i > 0 -> Some i
    | _ -> None

let updateInputSetting state setting =
    match setting with
    | Width w -> { state.inputSettings with width = w }
    | Height h -> { state.inputSettings with height = h }
    | Mines m -> { state.inputSettings with mines = m }
    |> fun input -> { state with inputSettings = input }

let assignMines count cells =
    let setMine cell = { cell with isMined = true }
    let shuffledCells = 
        cells // Shuffle cells a few times to make sure they are somewhat shuffled
        |> List.sortBy (fun _ -> rand.Next(0, 1000)) 
        |> List.sortBy (fun _ -> rand.Next(0, 1000)) 
        |> List.sortBy (fun _ -> rand.Next(0, 1000))
    let mined = shuffledCells |> List.take count |> List.map setMine
    mined @ (shuffledCells |> List.skip count) |> List.sortBy (fun c -> c.point.y, c.point.x)

let newGame (settings : Settings) =
    let axisPoints count = [ 0 .. (count - 1) ]
    let cells = 
        List.allPairs (axisPoints settings.width) (axisPoints settings.height) 
        |> List.map (Point.Create >> Cell.Create) 
        |> assignMines settings.mines

    { inputSettings = settings
      currentSettings = settings
      cells = cells
      status = InProgress }

let init() =
    newGame Settings.Default, Cmd.none

let isGameOver = function
    | Won | Lost -> true
    | InProgress -> false

let invalidSettings settings =
    settings.mines > settings.width * settings.height 
    || settings.width > maxWidth 
    || settings.height > maxHeight
    || settings.width <= 0
    || settings.height <= 0
    || settings.mines <= 0

let updateCell point (f : Cell -> Cell) =
    List.map (fun c -> if c.point = point then f c else c)

let flagCell state point =
    let flag cell = { cell with state = Flagged }
    { state with cells = state.cells |> updateCell point flag }

let unflagCell state point =
    let unflag cell = { cell with state = Unopened }
    { state with cells = state.cells |> updateCell point unflag }

let adjacentCells state point =
    List.allPairs [ -1; 0; 1 ] [ -1; 0; 1 ]
    |> List.where (fun (x, y) -> x <> 0 || y <> 0)
    |> List.map (
        fun (x,y) -> Point.Create(point.x + x, point.y + y)
        >> fun p -> state.cells |> List.tryFind (fun c -> c.point = p))
    |> List.choose id

let countAdjacentMines state point =
    adjacentCells state point
    |> List.where (fun c -> c.isMined)
    |> List.length

let countUnopenedAndFlaggedCells state =
    state.cells 
    |> List.choose (fun cell -> 
        match cell.state with
        | Unopened | Flagged -> Some cell
        | _ -> None)
    |> List.length

let countFlaggedCells state =
    state.cells 
    |> List.choose (fun cell -> 
        match cell.state with
        | Flagged -> Some cell
        | _ -> None)
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
    let state = openCell state point
    if state.cells |> List.exists (fun cell -> cell.point = point && cell.isMined) then 
        state, Cmd.ofMsg GameLost
    elif countUnopenedAndFlaggedCells state = state.currentSettings.mines then
        state, Cmd.ofMsg GameWon
    else
        let cellIsUnopenedAndNotMined cell = 
            if cell.isMined then false 
            else  match cell.state with | Unopened -> true | _ -> false
        let rec openAdjacentEmptyCells point state =
            adjacentCells state point
            |> List.where cellIsUnopenedAndNotMined
            |> List.fold (fun accState cell ->
                let count = countAdjacentMines accState cell.point
                if count > 0 then openCell accState cell.point
                else openCell accState cell.point |> openAdjacentEmptyCells cell.point) state

        match state.cells |> List.pick (fun cell -> if cell.point = point then Some cell.state else None) with
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
    | NewGame -> newGame state.inputSettings, Cmd.none
    | GameLost -> { state with status = Lost }, Cmd.none
    | GameWon -> { state with status = Won }, Cmd.none
    | SetWidth width -> updateInputSetting state (Width width), Cmd.none
    | SetHeight height -> updateInputSetting state (Height height), Cmd.none
    | SetMinesCount mines -> updateInputSetting state (Mines mines), Cmd.none

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
        prop.style [ style.margin 1; style.height 30; style.width 30]
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
        prop.classes [ "columns"; "is-paddingless"; "is-marginless" ]
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

let renderTitle =
    Html.p [ 
        prop.className "title"
        prop.style [ style.textAlign.center ]
        prop.text "Mine Sweeper"
    ]

let renderMinesLeft state =
    Html.p [ 
        prop.className "subtitle"
        prop.style [ style.textAlign.center ]
        prop.text (sprintf "%i Mines left" (state.currentSettings.mines - countFlaggedCells state))
    ]

let renderGameLost =
    Html.p [ 
        prop.classes [ "subtitle"; "has-text-danger"  ]
        prop.style [ style.textAlign.center ]
        prop.text "Game over"
    ]

let renderGameWon =
    Html.p [ 
        prop.classes [ "subtitle"; "has-text-success" ]
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

let renderNewGameForm settings dispatch =
    Html.div [
        prop.classes [ "column"; "is-narrow" ]
        prop.children [
            renderLabel "Width"
            renderInput "Width" settings.width 50 2 SetWidth dispatch
            renderLabel "Height"
            renderInput "Height" settings.height 50 2 SetHeight dispatch
            renderLabel "Mines"
            renderInput "Mines" settings.mines (settings.width * settings.height) 3 SetMinesCount dispatch
            Html.button [
                prop.classes [ "button"; "is-primary"; "is-light" ]
                prop.style [ style.marginTop 10; style.marginBottom 10 ]
                prop.type'.submit
                prop.text "New Game"
                prop.disabled (invalidSettings settings)
                prop.onClick (fun _ -> dispatch NewGame)
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.children [
            Html.div [
                renderTitle
                renderMinesLeft state
                Html.div [
                    prop.classes [ "columns"; "is-mobile" ]
                    prop.children [
                        Html.div [ 
                            prop.classes [ "column"; "is-narrow" ] 
                            prop.children [ 
                                renderNewGameForm state.inputSettings dispatch
                                match state.status with
                                | Won -> renderGameWon
                                | Lost -> renderGameLost
                                | InProgress -> Html.none ]
                        ]
                        Html.div [
                            prop.classes [ "column" ]
                            prop.children [ renderGrid state dispatch ]
                        ]
                    ]
                ]
            ]
        ]
    ]
