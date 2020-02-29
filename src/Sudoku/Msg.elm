module Sudoku.Msg exposing (Msg(..))

import Sudoku.Model exposing (BoxGroup, CValue, Cell, ModalCValue, Position)


type Msg
    = ValuesForBoxGenerated ( BoxGroup, BoxGroup ) (Maybe ( BoxGroup, BoxGroup )) (List CValue)
    | FreeCellSelected (List (Cell CValue)) (Cell CValue)
    | RemoveValueFromBoard (List Position)
    | RandomValueGenerated (List (Cell CValue)) Position CValue
    | DelayCommand (List (Cell CValue))
    | ShowModalWindow Position
    | ModalCommand (Cell ModalCValue)
