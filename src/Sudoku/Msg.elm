module Sudoku.Msg exposing (Msg(..))

import Sudoku.Model exposing (CValue, Cell, ModalCValue, Position)


type Msg
    = ValuesForDiagonalBoxesGenerated ( List CValue, List CValue, List CValue )
    | FreeCellSelected (List Position) Position
    | RemoveValueFromBoard (List Position)
    | RandomValueGenerated (List Position) Position CValue
    | DelayCommand (List Position)
    | ShowModalWindow Position
    | ModalCommand (Cell ModalCValue)
