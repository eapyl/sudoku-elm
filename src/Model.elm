module Model exposing (Board, BoxGroup(..), CValue(..), Cell, Index(..), Model, Position)


type BoxGroup
    = A
    | B
    | C


type CValue
    = Empty
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type alias Position =
    ( Index, Index )


type alias Cell =
    { pos : Position
    , value : CValue
    }


type Index
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Eighth
    | Ninth


type alias Board =
    List Cell


type alias Model =
    { board : Board
    , solution : Board
    , triedValues : List ( Position, CValue )
    , generationStatus : Maybe String
    , showModal : Bool
    }
