module Sudoku exposing
    ( allIndexes
    , allValues
    , boardSize
    , emptyModel
    , getCValue
    , getCell
    , getFreeCells
    , getUsedValues
    , hasAtLeastOneSolution
    , isValidValue
    , rawIndexToPossiblePosition
    , size
    , tryToRemoveValuesFromBoard
    )

import Convert exposing (fromInt, indexToInt)
import Model exposing (Board, CValue(..), Cell, Index(..), Level(..), Model, Position)


emptyModel : Model
emptyModel =
    Model initEmptyBoard initEmptyBoard [] Nothing [] Easy


size : Int
size =
    9


boardSize : Int
boardSize =
    size * size


getCValue : Board -> Position -> CValue
getCValue board ( row, column ) =
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
        |> Maybe.map (\c -> c.value)
        |> Maybe.withDefault Empty


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
