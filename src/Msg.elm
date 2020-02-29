module Msg exposing (Msg(..))

import Sudoku.Model as Sudoku
import Sudoku.Msg


type Msg
    = ChangeLevel Sudoku.Complexity
    | SudokuCommand Sudoku.Msg.Msg
