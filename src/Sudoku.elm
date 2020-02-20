module Sudoku exposing
    ( allValues
    , boardSize
    , emptyModel
    , fromInt
    , getCValue
    , getFreeCells
    , getUsedValues
    , hasAtLeastOneSolution
    , rawIndexToPossiblePosition
    , size
    , tryToRemoveValuesFromBoard
    )

import Model exposing (Board, CValue(..), Cell, Index(..), Model, Position)


emptyModel : Model
emptyModel =
    Model initEmptyBoard initEmptyBoard [] (Just "Generating")


size : Int
size =
    9


boardSize : Int
boardSize =
    size * size


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
getAllNonEmptyValuesInBox board pos =
    board
        |> List.filter (\c -> c.value /= Empty)
        |> List.filter
            (\c ->
                getBoxIndex pos == getBoxIndex c.pos
            )
        |> List.map (\c -> c.value)


getAllNonEmptyValuesInRow : Board -> Index -> List CValue
getAllNonEmptyValuesInRow =
    getAllNonEmptyValues True


getAllNonEmptyValuesInColumn : Board -> Index -> List CValue
getAllNonEmptyValuesInColumn =
    getAllNonEmptyValues False


getAllNonEmptyValues : Bool -> Board -> Index -> List CValue
getAllNonEmptyValues isRow board index =
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
        |> List.map (\c -> c.value)


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
