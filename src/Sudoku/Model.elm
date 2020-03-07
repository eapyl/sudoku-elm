module Sudoku.Model exposing
    ( Board
    , BoardCell
    , BoardCellType(..)
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


type BoardCellType
    = Initial
    | Valid
    | Invalid


type alias BoardCell =
    { position : Position
    , value : CValue
    , category : BoardCellType
    }


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
    List BoardCell


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
