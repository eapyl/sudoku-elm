module Model exposing (Model, emptyModel)

import Sudoku.Model as SudokuModel exposing (Model, Position)
import Sudoku.Update as SudokuUpdate exposing (emptyModel)


emptyModel : Model
emptyModel =
    Model SudokuUpdate.emptyModel Nothing


type alias Model =
    { sudoku : SudokuModel.Model
    , selectedCell : Maybe Position
    }
