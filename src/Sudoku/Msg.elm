module Sudoku.Msg exposing (Msg(..))

import Sudoku.Model exposing (CValue, Cell, ModalCValue, Position)


type Msg
    = ValuesForDiagonalBoxesGenerated ( List CValue, List CValue, List CValue )
    | FreeCellSelected (List (Cell CValue)) (Cell CValue)
    | RemoveValueFromBoard (List Position)
    | RandomValueGenerated (List (Cell CValue)) Position CValue
    | DelayCommand (List (Cell CValue))
    | ShowModalWindow Position
    | ModalCommand (Cell ModalCValue)
