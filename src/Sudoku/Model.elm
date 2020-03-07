module Sudoku.Model exposing
    ( Board
    , BoardCell
    , BoardCellType(..)
    , Complexity(..)
    , Index(..)
    , ModalCell
    , ModalValue(..)
    , Model
    , Position
    , Value(..)
    , boardSize
    , size
    )


type Value
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
    { row : Index
    , col : Index
    , box : Index
    }


type BoardCellType
    = Initial
    | Valid
    | Invalid


type alias BoardCell =
    { position : Position
    , value : Value
    , category : BoardCellType
    }


type alias ModalCell =
    { position : Position
    , value : ModalValue
    }


type ModalValue
    = Number Value
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
    , triedValues : List ( Position, Value )
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
