module Sudoku.Msg exposing (Msg(..))

import Sudoku.Model exposing (BoardCell, ModalCell, Position, Value)


type Msg
    = InitialValuesForBoardGenerated (List Value)
    | FreeCellSelected (List Position) (Maybe BoardCell)
    | RemoveValueFromBoard (List Position)
    | DelayCommand (List Position)
    | ShowModalWindow Position
    | ModalCommand ModalCell
