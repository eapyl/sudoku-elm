module Sudoku.Model exposing
    ( Board
    , BoxGroup(..)
    , CValue(..)
    , Cell
    , Complexity(..)
    , Index(..)
    , ModalCValue(..)
    , Model
    , Position
    , boardSize
    , size
    )


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


type alias Cell a =
    { pos : Position
    , value : a
    }


type ModalCValue
    = Number CValue
    | EmptyValue
    | Back


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
    List (Cell CValue)


type Complexity
    = Easy
    | Normal
    | Hard


type alias Model =
    { board : Board
    , solution : Board
    , freeCells : List Position
    , triedValues : List ( Position, CValue )
    , complexity : Complexity
    , selectedCell : Maybe Position
    , status : Maybe String
    }


size : Int
size =
    9


boardSize : Int
boardSize =
    size * size