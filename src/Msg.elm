module Msg exposing (Msg(..))

import Sudoku as Sudoku


type Msg
    = ShowModal Sudoku.Position
    | CloseModal
    | SelectedCValue Sudoku.Position Sudoku.CValue
    | ChangeLevel Sudoku.Complexity
    | SudokuCommand Sudoku.Msg
