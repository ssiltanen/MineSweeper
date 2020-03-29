module App

open Elmish
open Model
open Helpers

type Msg =
    | Initialized of State
    | ToggleFlag of Point * RenderState
    | OpenCell of Point * RenderState
    | GameLost
    | GameWon
    | NewGame
    | SetWidth of int
    | SetHeight of int
    | SetMinesCount of int

let updateInputSetting state setting =
    match setting with
    | Width w -> { state.inputSettings with width = w }
    | Height h -> { state.inputSettings with height = h }
    | Mines m -> { state.inputSettings with mines = m }
    |> fun input -> { state with inputSettings = input }

let adjacentCells cells point =
    List.allPairs [ -1; 0; 1 ] [ -1; 0; 1 ]
    |> List.where (fun (x, y) -> x <> 0 || y <> 0)
    |> List.map (
        fun (x,y) -> Point.Create(point.x + x, point.y + y)
        >> fun p -> cells |> List.tryFind (fun c -> c.point = p))
    |> List.choose id

let countAdjacentMines cells point =
    adjacentCells cells point
    |> List.where (fun c -> c.isMined)
    |> List.length

let assignMines count cells =
    let setMine cell = { cell with isMined = true }
    let shuffledCells = 
        cells // Shuffle cells a few times to make sure they are somewhat shuffled
        |> List.sortBy (fun _ -> random.Next(0, 1000)) 
        |> List.sortBy (fun _ -> random.Next(0, 1000)) 
        |> List.sortBy (fun _ -> random.Next(0, 1000))
    let mined = shuffledCells |> List.take count |> List.map setMine
    let cells = mined @ (shuffledCells |> List.skip count)
    cells 
    |> List.map (fun cell -> { cell with adjacentMines = countAdjacentMines cells cell.point })
    |> List.sortBy (fun c -> c.point.y, c.point.x)

let newGame (settings : Settings) =
    let axisPoints count = [ 0 .. (count - 1) ]
    let cells = 
        List.allPairs (axisPoints settings.width) (axisPoints settings.height) 
        |> List.map (Point.Create >> Cell.Create) 
        |> assignMines settings.mines

    { inputSettings = settings
      currentSettings = settings
      cells = cells
      openedCellCount = 0
      status = InProgress }

let newGameAsync (settings : Settings) =
    let asyncInit (dispatch: Msg -> unit) : unit =
        async {
            do! Async.Sleep 40
            let status = newGame settings
            dispatch (Initialized status)
        }
        |> Async.StartImmediate
    Cmd.ofSub asyncInit

let isGameOver = function
    | InProgress -> false
    | _ -> true

let isInitializing = function
    | Initializing -> true
    | _ -> false

let invalidSettings settings =
    settings.mines > settings.width * settings.height 
    || settings.width > maxWidth 
    || settings.height > maxHeight
    || settings.width <= 0
    || settings.height <= 0
    || settings.mines <= 0

let updateCell (f : Cell -> Cell) point =
    List.map (fun c -> if c.point = point then f c else c)

let flagCell state point =
    let flag cell = { cell with state = Flagged }
    { state with cells = state.cells |> updateCell flag point }

let unflagCell state point =
    let unflag cell = { cell with state = Unopened }
    { state with cells = state.cells |> updateCell unflag point }

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
                if cell.adjacentMines = 0 then Empty
                else HasAdjacentMines cell.adjacentMines 
        { cell with state = newState }
    { state with 
        openedCellCount = state.openedCellCount + 1
        cells = state.cells |> updateCell openCell point}

let openCellAndEmptyAdjacentCells state point =
    let state = openCell state point
    if state.cells |> List.exists (fun cell -> cell.point = point && cell.isMined) then 
        state, Cmd.ofMsg GameLost
    elif List.length state.cells = state.openedCellCount + state.currentSettings.mines then
        state, Cmd.ofMsg GameWon
    else
        let cellIsUnopened cell = match cell.state with | Unopened -> true | _ -> false
        let rec openAdjacentEmptyCells point state =
            adjacentCells state.cells point
            |> List.where cellIsUnopened
            |> List.fold (fun accState cell ->
                let newState = openCell accState cell.point
                if cell.adjacentMines = 0 then openAdjacentEmptyCells cell.point newState
                else newState) state

        match state.cells |> List.pick (fun cell -> if cell.point = point then Some cell.state else None) with
        | Empty -> openAdjacentEmptyCells point state, Cmd.none
        | HasAdjacentMines _ -> state, Cmd.none
        | state -> failwithf "Error: Opened cell had invalid state %A" state

let init () =
    newGame Settings.Default, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | ToggleFlag (point, Unopened) -> flagCell state point, Cmd.none
    | ToggleFlag (point, Flagged) -> unflagCell state point, Cmd.none
    | ToggleFlag _ -> state, Cmd.none
    | OpenCell (point, Unopened) -> openCellAndEmptyAdjacentCells state point
    | OpenCell _ -> state, Cmd.none
    | NewGame -> { state with status = Initializing }, newGameAsync state.inputSettings
    | GameLost -> { state with status = Lost }, Cmd.none
    | GameWon -> { state with status = Won }, Cmd.none
    | SetWidth width -> updateInputSetting state (Width width), Cmd.none
    | SetHeight height -> updateInputSetting state (Height height), Cmd.none
    | SetMinesCount mines -> updateInputSetting state (Mines mines), Cmd.none
    | Initialized state -> state, Cmd.none