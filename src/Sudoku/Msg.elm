module Sudoku.Msg exposing (Msg(..))

import Sudoku.Model exposing (ModalCell, Position, Value)


type Msg
    = InitialValuesForBoardGenerated (List Value)
    | FreeCellSelected (List Position) Position
    | RemoveValueFromBoard (List Position)
    | RandomValueGenerated (List Position) Position Value
    | DelayCommand (List Position)
    | ShowModalWindow Position
    | ModalCommand ModalCell
