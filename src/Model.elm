module Model exposing (Level(..), Model, emptyModel)

import Sudoku exposing (Model, Position, emptyModel)


emptyModel : Model
emptyModel =
    Model Sudoku.emptyModel Nothing Easy


type Level
    = Easy
    | Normal
    | Hard


type alias Model =
    { sudoku : Sudoku.Model
    , selectedCell : Maybe Position
    , level : Level
    }
