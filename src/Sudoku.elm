module Sudoku exposing
    ( Board
    , CValue(..)
    , Cell
    , Complexity(..)
    , Index
    , Model
    , Msg
    , Position
    , allIndexes
    , allValues
    , createBoard
    , emptyModel
    , fromInt
    , getCell
    , getFreeCells
    , indexToInt
    , initEmptyBoard
    , isValidValue
    , setCell
    , setComplexity
    , toString
    , update
    )

import Process
import Random
import Random.List
import Task


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
    , status : Maybe String
    }


emptyModel : Model
emptyModel =
    Model initEmptyBoard initEmptyBoard [] [] Easy (Just "Generating board")


type Msg
    = ValuesForBoxGenerated ( BoxGroup, BoxGroup ) (Maybe ( BoxGroup, BoxGroup )) (List CValue)
    | FreeCellSelected (List Cell) Cell
    | RemoveValueFromBoard (List Position)
    | RandomValueGenerated (List Cell) Position CValue
    | DelayCommand (List Cell)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValuesForBoxGenerated boxPosition nextBoxPosition values ->
            let
                getMultiplier ( mainGroup, _ ) =
                    case mainGroup of
                        A ->
                            0

                        B ->
                            3

                        C ->
                            6

                updatedValues =
                    let
                        multiplier =
                            getMultiplier boxPosition
                    in
                    values
                        |> List.indexedMap
                            (\i ->
                                \value ->
                                    let
                                        row =
                                            (i + 3 * multiplier) // 3 |> fromInt

                                        col =
                                            (remainderBy 3 i + multiplier) |> fromInt
                                    in
                                    case ( row, col ) of
                                        ( Just r, Just c ) ->
                                            Just ( ( r, c ), value )

                                        _ ->
                                            Nothing
                            )

                updatedBoard =
                    model.board
                        |> List.map
                            (\c ->
                                let
                                    searchedValue =
                                        List.filterMap
                                            (\i ->
                                                case i of
                                                    Just ( pos, value ) ->
                                                        if c.pos == pos then
                                                            Just ( pos, value )

                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing
                                            )
                                            updatedValues
                                            |> List.head
                                in
                                case searchedValue of
                                    Just ( _, value ) ->
                                        { c | value = value }

                                    Nothing ->
                                        c
                            )
            in
            ( { model | solution = updatedBoard, board = updatedBoard }
            , case nextBoxPosition of
                Just ( B, B ) ->
                    Random.generate (ValuesForBoxGenerated ( B, B ) (Just ( C, C ))) valueCompleteGenerator

                Just ( C, C ) ->
                    Random.generate (ValuesForBoxGenerated ( C, C ) Nothing) valueCompleteGenerator

                _ ->
                    generateBoard <| getFreeCells updatedBoard
            )

        FreeCellSelected freeCells cell ->
            let
                usedValues =
                    getUsedValues model.board cell.pos

                getPossibleValuesForCell =
                    allValues
                        |> List.filter (\a -> List.member a usedValues |> not)

                command =
                    case getPossibleValuesForCell of
                        head :: tail ->
                            Random.generate
                                (RandomValueGenerated freeCells cell.pos)
                                (valueGenerator head tail)

                        [] ->
                            Cmd.none
            in
            ( model
            , command
            )

        RemoveValueFromBoard positionsToClean ->
            let
                mapLevelToInt =
                    case model.complexity of
                        Hard ->
                            60

                        Normal ->
                            40

                        Easy ->
                            20

                boardWithFreeCells =
                    tryToRemoveValuesFromBoard (List.take mapLevelToInt positionsToClean) model.board

                freeCellsPositions =
                    boardWithFreeCells
                        |> List.filter (\x -> x.value == Empty)
                        |> List.map (\x -> x.pos)
            in
            ( { model
                | board = boardWithFreeCells
                , freeCells = freeCellsPositions
              }
            , Cmd.none
            )

        RandomValueGenerated freeCells cellPos randomValue ->
            let
                filteredCells =
                    freeCells
                        |> List.filter (\c -> c.pos /= cellPos)

                updatedBoard =
                    model.board
                        |> List.map
                            (\c ->
                                if c.pos == cellPos then
                                    { c | value = randomValue }

                                else
                                    c
                            )

                oneSolution =
                    hasAtLeastOneSolution updatedBoard filteredCells

                ( updatedModel, remainingCells ) =
                    if List.member ( cellPos, randomValue ) model.triedValues then
                        ( model.board, freeCells )

                    else if oneSolution then
                        ( updatedBoard, filteredCells )

                    else
                        ( model.board, freeCells )
            in
            ( { model
                | solution = updatedModel
                , board = updatedModel
                , triedValues = ( cellPos, randomValue ) :: model.triedValues
                , status =
                    if List.isEmpty remainingCells then
                        Nothing

                    else
                        Just <| (((54 - List.length freeCells) * 100 // 54) |> String.fromInt) ++ "%"
              }
            , sendDelayed DelayCommand remainingCells
            )

        DelayCommand remainingCells ->
            ( model
            , generateBoard remainingCells
            )


sendDelayed : (a -> msg) -> a -> Cmd msg
sendDelayed msg a =
    Process.sleep 20
        |> Task.perform (\_ -> msg a)


generateBoard : List Cell -> Cmd Msg
generateBoard freeCells =
    case freeCells of
        head :: tail ->
            Random.generate (FreeCellSelected freeCells) <| freeCellGenerator head tail

        [] ->
            Random.generate RemoveValueFromBoard positionCompleteGenerator


freeCellGenerator : Cell -> List Cell -> Random.Generator Cell
freeCellGenerator initialCell otherFreeCells =
    Random.uniform initialCell otherFreeCells


positionCompleteGenerator : Random.Generator (List Position)
positionCompleteGenerator =
    List.range 0 (boardSize - 1)
        |> List.map rawIndexToPossiblePosition
        |> List.filterMap
            (\( mr, mc ) ->
                case ( mr, mc ) of
                    ( Just r, Just c ) ->
                        Just ( r, c )

                    _ ->
                        Nothing
            )
        |> Random.List.shuffle


valueGenerator : CValue -> List CValue -> Random.Generator CValue
valueGenerator initial rest =
    Random.uniform initial rest


createBoardCommand : List CValue -> Msg
createBoardCommand =
    ValuesForBoxGenerated ( A, A ) (Just ( B, B ))


createBoard : Cmd Msg
createBoard =
    Random.generate createBoardCommand valueCompleteGenerator


valueCompleteGenerator : Random.Generator (List CValue)
valueCompleteGenerator =
    Random.List.shuffle allValues


size : Int
size =
    9


boardSize : Int
boardSize =
    size * size


getCell : Board -> Position -> Maybe Cell
getCell board ( row, column ) =
    board
        |> List.filter
            (\c ->
                let
                    ( r, col ) =
                        c.pos
                in
                r == row && col == column
            )
        |> List.head


setCell : Model -> Cell -> Model
setCell model cell =
    let
        newBoard =
            model.board
                |> List.map
                    (\c ->
                        if c.pos == cell.pos then
                            { c | value = cell.value }

                        else
                            c
                    )
    in
    { model
        | board = newBoard
        , status =
            if newBoard == model.solution then
                Just "Solved!"

            else
                Nothing
    }


type SolutionCount
    = Zero
    | Single
    | More


hasOnlyOneSolution : Board -> List Cell -> Bool
hasOnlyOneSolution board freeCells =
    case backtracking False Zero board freeCells of
        Zero ->
            False

        Single ->
            True

        More ->
            False


hasAtLeastOneSolution : Board -> List Cell -> Bool
hasAtLeastOneSolution board freeCells =
    case backtracking True Zero board freeCells of
        Zero ->
            False

        _ ->
            True


backtracking : Bool -> SolutionCount -> Board -> List Cell -> SolutionCount
backtracking oneSolutionEnough count board freeCells =
    case freeCells of
        head :: tail ->
            let
                usedValues =
                    getUsedValues board head.pos

                tryValuesForCell solutionCount possibleValues =
                    case possibleValues of
                        possibleValue :: otherValues ->
                            let
                                updatedBoard =
                                    board
                                        |> List.map
                                            (\c ->
                                                if c.pos == head.pos then
                                                    { c | value = possibleValue }

                                                else
                                                    c
                                            )
                            in
                            case backtracking oneSolutionEnough solutionCount updatedBoard tail of
                                Zero ->
                                    tryValuesForCell Zero otherValues

                                Single ->
                                    if oneSolutionEnough then
                                        Single

                                    else
                                        tryValuesForCell Single otherValues

                                More ->
                                    More

                        [] ->
                            solutionCount
            in
            allValues
                |> List.filter (\a -> List.member a usedValues |> not)
                |> tryValuesForCell count

        [] ->
            case count of
                Zero ->
                    Single

                Single ->
                    More

                More ->
                    More


tryToRemoveValuesFromBoard : List Position -> Board -> Board
tryToRemoveValuesFromBoard positionsToClean board =
    case positionsToClean of
        currentPositionToClean :: restPositions ->
            let
                updatedBoard =
                    board
                        |> List.map
                            (\c ->
                                if c.pos == currentPositionToClean then
                                    { c | value = Empty }

                                else
                                    c
                            )

                onlyOneSolution =
                    hasOnlyOneSolution updatedBoard <| getFreeCells updatedBoard
            in
            if onlyOneSolution then
                tryToRemoveValuesFromBoard restPositions updatedBoard

            else
                tryToRemoveValuesFromBoard restPositions board

        [] ->
            board


getUsedValues : Board -> Position -> List CValue
getUsedValues board ( row, col ) =
    let
        inBox =
            getAllNonEmptyValuesInBox board ( row, col )

        inRow =
            getAllNonEmptyValuesInRow board row
                |> List.filter (\a -> List.member a inBox |> not)

        inCol =
            getAllNonEmptyValuesInColumn board col
                |> List.filter (\a -> List.member a inBox |> not)
                |> List.filter (\a -> List.member a inRow |> not)
    in
    List.append inBox inRow
        |> List.append inCol


getBoxIndex : Position -> Int
getBoxIndex ( row, col ) =
    let
        rowInd =
            indexToInt row

        colInd =
            indexToInt col
    in
    rowInd // 3 * 3 + colInd // 3


getAllNonEmptyValuesInBox : Board -> Position -> List CValue
getAllNonEmptyValuesInBox =
    getAllNonEmptyValuesInBoxWithSkip Nothing


getAllNonEmptyValuesInBoxWithSkip : Maybe Position -> Board -> Position -> List CValue
getAllNonEmptyValuesInBoxWithSkip skipCell board pos =
    board
        |> List.filter (\c -> c.value /= Empty)
        |> List.filter
            (\c ->
                getBoxIndex pos == getBoxIndex c.pos
            )
        |> List.filter
            (\c ->
                case skipCell of
                    Just v ->
                        v /= c.pos

                    Nothing ->
                        True
            )
        |> List.map (\c -> c.value)


getAllNonEmptyValuesInRow : Board -> Index -> List CValue
getAllNonEmptyValuesInRow =
    getAllNonEmptyValues True


getAllNonEmptyValuesInColumn : Board -> Index -> List CValue
getAllNonEmptyValuesInColumn =
    getAllNonEmptyValues False


getAllNonEmptyValues : Bool -> Board -> Index -> List CValue
getAllNonEmptyValues =
    getAllNonEmptyValuesWithSkip Nothing


getAllNonEmptyValuesWithSkip : Maybe Index -> Bool -> Board -> Index -> List CValue
getAllNonEmptyValuesWithSkip skipRowOrCol isRow board index =
    board
        |> List.filter (\c -> c.value /= Empty)
        |> List.filter
            (\c ->
                let
                    ( row, col ) =
                        c.pos
                in
                if isRow then
                    row == index

                else
                    col == index
            )
        |> List.filter
            (\c ->
                let
                    ( row, col ) =
                        c.pos
                in
                case skipRowOrCol of
                    Just v ->
                        if isRow then
                            v /= col

                        else
                            v /= row

                    Nothing ->
                        True
            )
        |> List.map (\c -> c.value)


setComplexity : Model -> Complexity -> Model
setComplexity model complexity =
    { model | complexity = complexity }


rawIndexToPossiblePosition : Int -> ( Maybe Index, Maybe Index )
rawIndexToPossiblePosition i =
    let
        row =
            i // size

        column =
            remainderBy size i
    in
    ( fromInt row, fromInt column )


initEmptyBoard : Board
initEmptyBoard =
    List.range 0 (boardSize - 1)
        |> List.map (\i -> rawIndexToPossiblePosition i)
        |> List.filterMap
            (\( r, c ) ->
                case ( r, c ) of
                    ( Just rV, Just cV ) ->
                        Just ( rV, cV )

                    _ ->
                        Nothing
            )
        |> List.map (\pos -> Cell pos Empty)


getFreeCells : Board -> List Cell
getFreeCells board =
    board
        |> List.filter (\c -> c.value == Empty)


allValues : List CValue
allValues =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]


allIndexes : List Index
allIndexes =
    [ First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth, Ninth ]


isValidValue : Board -> Cell -> Bool
isValidValue board cell =
    let
        ( row, col ) =
            cell.pos
    in
    (getAllNonEmptyValuesWithSkip (Just col) True board row |> List.member cell.value |> not)
        && (getAllNonEmptyValuesWithSkip (Just row) False board col |> List.member cell.value |> not)
        && (getAllNonEmptyValuesInBoxWithSkip (Just cell.pos) board cell.pos |> List.member cell.value |> not)


fromInt : Int -> Maybe Index
fromInt index =
    case index of
        0 ->
            Just First

        1 ->
            Just Second

        2 ->
            Just Third

        3 ->
            Just Fourth

        4 ->
            Just Fifth

        5 ->
            Just Sixth

        6 ->
            Just Seventh

        7 ->
            Just Eighth

        8 ->
            Just Ninth

        _ ->
            Nothing


toString : CValue -> String
toString value =
    case value of
        Empty ->
            " "

        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"


indexToInt : Index -> Int
indexToInt index =
    case index of
        First ->
            0

        Second ->
            1

        Third ->
            2

        Fourth ->
            3

        Fifth ->
            4

        Sixth ->
            5

        Seventh ->
            6

        Eighth ->
            7

        Ninth ->
            8
