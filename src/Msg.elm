module Msg exposing (Msg(..))

import Model exposing (BoxGroup, CValue, Cell, Position)


type Msg
    = GenerateBoard
    | FreeCellSelected (List Cell) Cell
    | RandomValueGenerated (List Cell) Position CValue
    | ValuesForBoxGenerated ( BoxGroup, BoxGroup ) (Maybe ( BoxGroup, BoxGroup )) (List CValue)
    | RemoveValueFromBoard (List Position)
