module Msg exposing (Msg(..))

import Model exposing (Level)
import Sudoku as Sudoku


type Msg
    = ShowModal Sudoku.Position
    | CloseModal
    | SelectedCValue Sudoku.Position Sudoku.CValue
    | ChangeLevel Level
    | SudokuCommand Sudoku.Msg
