module Msg exposing (Msg(..))

import Model exposing (BoxGroup, CValue, Cell, Level, Position)


type Msg
    = GenerateBoard
    | FreeCellSelected (List Cell) Cell
    | RandomValueGenerated (List Cell) Position CValue
    | ValuesForBoxGenerated ( BoxGroup, BoxGroup ) (Maybe ( BoxGroup, BoxGroup )) (List CValue)
    | RemoveValueFromBoard (List Position)
    | ShowModal Position
    | CloseModal
    | SelectedCValue Position CValue
    | ChangeLevel Level
