module Model exposing (Model, emptyModel)

import Sudoku exposing (Model, Position, emptyModel)


emptyModel : Model
emptyModel =
    Model Sudoku.emptyModel Nothing


type alias Model =
    { sudoku : Sudoku.Model
    , selectedCell : Maybe Position
    }
