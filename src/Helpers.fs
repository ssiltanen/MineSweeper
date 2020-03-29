module Helpers

open System

let random = Random()

let tryParseAboveZeroInt (str  : string) = 
    match Int32.TryParse(str) with
    | true, i when i > 0 -> Some i
    | _ -> None
   
let totalSecondsDifference (a : DateTime) (b : DateTime) =
    (b - a).TotalSeconds |> int