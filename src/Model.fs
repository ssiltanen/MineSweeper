module Model

type RenderState = Unopened | Empty | HasAdjacentMines of int | Flagged | Mine
type Point = { x : int; y : int } with
    static member Create (x, y) = { x = x; y = y }
type Cell = { point : Point; state : RenderState; adjacentMines : int; isMined : bool } with
    static member Create point = { point = point; state = Unopened; adjacentMines = 0; isMined = false }

type Settings = { width : int; height : int; mines : int } with
    static member Default = { width = 30; height = 30; mines = 5 }

let maxWidth = 40
let maxHeight = 40

type GameStatus =
    | Initializing
    | InProgress
    | Won
    | Lost

type State = 
    { inputSettings : Settings
      currentSettings : Settings
      cells : Cell list
      openedCellCount : int
      status : GameStatus }

type InputSetting =
    | Width of int
    | Height of int
    | Mines of int