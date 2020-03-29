module App

open System
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
    | Tick

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
      status = InProgress
      startTime = DateTime.Now
      currentTime = DateTime.Now }

let newGameAsync (settings : Settings) =
    let asyncInit (dispatch: Msg -> unit) : unit =
        async {
            do! Async.Sleep 40
            let freshState = newGame settings
            dispatch (Initialized freshState)
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

let getCell point =
    List.find (fun c -> c.point = point)

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

let openCell state cell =
    match cell.state with
    | Unopened ->
        let openCell cell =
            let newState =
                if cell.isMined then Mine
                else 
                    if cell.adjacentMines = 0 then Empty
                    else HasAdjacentMines cell.adjacentMines 
            { cell with state = newState }
        let cells = state.cells |> updateCell openCell cell.point
        let cell = getCell cell.point cells
        { state with 
            openedCellCount = state.openedCellCount + 1
            cells = cells }, cell
    | _ -> state, cell

let openMinedCells state =
    state.cells
    |> List.where (fun cell -> cell.isMined)
    |> List.fold (fun a c -> openCell a c |> fst) state

let openAvailableCells state point =
    let (state, openedCell) = getCell point state.cells |> openCell state
    if openedCell.isMined then 
        openMinedCells state, Cmd.ofMsg GameLost
    else
        let cellIsUnopened cell = match cell.state with | Unopened -> true | _ -> false
        let rec openAdjacentEmptyCells point state processedCells =
            adjacentCells state.cells point
            |> List.where cellIsUnopened
            |> List.fold (fun (accState, processed) cell ->
                if processed |> List.exists (fun c -> c.point = cell.point) then accState, processed
                else
                    let (newState, cell) = openCell accState cell
                    let processed = cell :: processed
                    if cell.adjacentMines = 0 
                    then openAdjacentEmptyCells cell.point newState processed
                    else newState, processed) (state, processedCells)

        match openedCell.state with
        | Empty -> openAdjacentEmptyCells point state [ openedCell ] |> fst
        | HasAdjacentMines _ -> state
        | state -> failwithf "Opened cell had invalid state %A" state
        |> fun state -> 
            if List.length state.cells = state.openedCellCount + state.currentSettings.mines
            then state, Cmd.ofMsg GameWon
            else state, Cmd.none

let tick =
    let step (dispatch: Msg -> unit) : unit = 
        async {
            do! Async.Sleep 1000
            dispatch Tick
        }
        |> Async.StartImmediate
    
    Cmd.ofSub step

let init () =
    newGame Settings.Default, Cmd.ofMsg Tick

let update (msg: Msg) (state: State) =
    match msg with
    | ToggleFlag (point, Unopened) -> flagCell state point, Cmd.none
    | ToggleFlag (point, Flagged) -> unflagCell state point, Cmd.none
    | ToggleFlag _ -> state, Cmd.none
    | OpenCell (point, Unopened) -> openAvailableCells state point
    | OpenCell _ -> state, Cmd.none
    | NewGame -> { state with status = Initializing }, newGameAsync state.inputSettings
    | GameLost -> { state with status = Lost }, Cmd.none
    | GameWon -> { state with status = Won }, Cmd.none
    | SetWidth width -> updateInputSetting state (Width width), Cmd.none
    | SetHeight height -> updateInputSetting state (Height height), Cmd.none
    | SetMinesCount mines -> updateInputSetting state (Mines mines), Cmd.none
    | Initialized state -> state, tick
    | Tick -> 
        match state.status with
        | InProgress -> { state with currentTime = DateTime.Now }, tick
        | _ -> state, Cmd.none