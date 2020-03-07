module Sudoku.Update exposing (createBoard, emptyModel, setComplexity, update)

import Process
import Random
import Random.List
import Sudoku.Model
    exposing
        ( Board
        , BoardCell
        , BoardCellType(..)
        , CValue(..)
        , Complexity(..)
        , Index
        , ModalCValue(..)
        , Model
        , Position
        , boardSize
        , size
        )
import Sudoku.Msg exposing (Msg(..))
import Sudoku.Utils exposing (allValues, fromInt, indexToInt)
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValuesForDiagonalBoxesGenerated ( list1, list2, list3 ) ->
            let
                generatedCellValuesForOneBox multiplier list =
                    list
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
                        |> List.filterMap (\a -> a)

                allGeneratedValues =
                    List.append (generatedCellValuesForOneBox 0 list1) <|
                        List.append (generatedCellValuesForOneBox 3 list2) (generatedCellValuesForOneBox 6 list3)

                updatedBoard =
                    model.board
                        |> List.map
                            (\boardCell ->
                                allGeneratedValues
                                    |> List.filterMap
                                        (\( pos, value ) ->
                                            if boardCell.position == pos then
                                                Just <| BoardCell pos value Initial

                                            else
                                                Nothing
                                        )
                                    |> List.head
                                    |> Maybe.withDefault boardCell
                            )
            in
            ( { model | board = updatedBoard }
            , generateBoard <| getPositionsOfFreeCells updatedBoard
            )

        FreeCellSelected freeCellPositions position ->
            let
                usedValues =
                    getUsedValues model.board position

                getPossibleValuesForCell =
                    allValues
                        |> List.filter (\a -> List.member a usedValues |> not)

                command =
                    case getPossibleValuesForCell of
                        head :: tail ->
                            Random.generate
                                (RandomValueGenerated freeCellPositions position)
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
                            50

                        Normal ->
                            35

                        Easy ->
                            20

                boardWithFreeCells =
                    tryToRemoveValuesFromBoard (List.take mapLevelToInt positionsToClean) model.board

                freeCellsPositions =
                    boardWithFreeCells
                        |> List.filter (\x -> x.value == Empty)
                        |> List.map (\x -> x.position)
            in
            ( { model
                | board = boardWithFreeCells
                , freeCells = freeCellsPositions
                , status = Nothing
              }
            , Cmd.none
            )

        RandomValueGenerated freeCellPositions cellPos randomValue ->
            let
                filteredCells =
                    freeCellPositions
                        |> List.filter (\c -> c /= cellPos)

                updatedBoard =
                    updateCellOnBoard model.board (BoardCell cellPos randomValue Initial)

                oneSolution =
                    hasAtLeastOneSolution updatedBoard filteredCells

                ( updatedModel, remainingCells ) =
                    if List.member ( cellPos, randomValue ) model.triedValues then
                        ( model.board, freeCellPositions )

                    else if oneSolution then
                        ( updatedBoard, filteredCells )

                    else
                        ( model.board, freeCellPositions )
            in
            ( { model
                | solution = updatedModel
                , board = updatedModel
                , triedValues = ( cellPos, randomValue ) :: model.triedValues
                , status =
                    if List.isEmpty remainingCells then
                        Just "Finalizing"

                    else
                        Just <| (((54 - List.length freeCellPositions) * 100 // 54) |> String.fromInt) ++ "%"
              }
            , sendDelayed DelayCommand remainingCells
            )

        DelayCommand remainingCells ->
            ( model
            , generateBoard remainingCells
            )

        ShowModalWindow selectedPos ->
            ( { model | selectedCell = Just selectedPos }
            , Cmd.none
            )

        ModalCommand cell ->
            case cell.value of
                Number selectedCValue ->
                    let
                        isValid =
                            isValidValue model.board ( cell.pos, selectedCValue )

                        newBoard =
                            if isValid then
                                updateCellOnBoard model.board (BoardCell cell.pos selectedCValue Valid)

                            else
                                updateCellOnBoard model.board (BoardCell cell.pos selectedCValue Invalid)
                    in
                    ( { model
                        | board = newBoard
                        , selectedCell = Nothing
                        , status =
                            if newBoard == model.solution then
                                Just "Solved!"

                            else
                                Nothing
                      }
                    , Cmd.none
                    )

                Back ->
                    ( { model | selectedCell = Nothing }
                    , Cmd.none
                    )

                EmptyValue ->
                    ( model
                    , Cmd.none
                    )


sendDelayed : (a -> msg) -> a -> Cmd msg
sendDelayed msg a =
    Process.sleep 20
        |> Task.perform (\_ -> msg a)


generateBoard : List Position -> Cmd Msg
generateBoard freeCellPositions =
    case freeCellPositions of
        head :: tail ->
            Random.generate (FreeCellSelected freeCellPositions) <| freeCellGenerator head tail

        [] ->
            Random.generate RemoveValueFromBoard positionCompleteGenerator


valueGenerator : CValue -> List CValue -> Random.Generator CValue
valueGenerator initial rest =
    Random.uniform initial rest


createBoard : Cmd Msg
createBoard =
    Random.generate ValuesForDiagonalBoxesGenerated valueCompleteGenerator


freeCellGenerator : Position -> List Position -> Random.Generator Position
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


valueCompleteGenerator : Random.Generator ( List CValue, List CValue, List CValue )
valueCompleteGenerator =
    let
        listGenerator =
            Random.List.shuffle allValues
    in
    Random.map3
        (\a -> \b -> \c -> ( a, b, c ))
        listGenerator
        listGenerator
        listGenerator


updateCellOnBoard : Board -> BoardCell -> Board
updateCellOnBoard board cell =
    board
        |> List.map
            (\c ->
                if c.position == cell.position then
                    { c | value = cell.value }

                else
                    c
            )


type SolutionCount
    = Zero
    | Single
    | More


hasOnlyOneSolution : Board -> List Position -> Bool
hasOnlyOneSolution board freeCellPositions =
    case backtracking False Zero board freeCellPositions of
        Single ->
            True

        _ ->
            False


hasAtLeastOneSolution : Board -> List Position -> Bool
hasAtLeastOneSolution board freeCellPositions =
    case backtracking True Zero board freeCellPositions of
        Zero ->
            False

        _ ->
            True


backtracking : Bool -> SolutionCount -> Board -> List Position -> SolutionCount
backtracking oneSolutionEnough count board freeCellPositions =
    case freeCellPositions of
        head :: tail ->
            let
                usedValues =
                    getUsedValues board head

                tryValuesForCell solutionCount possibleValues =
                    case possibleValues of
                        possibleValue :: otherValues ->
                            let
                                updatedBoard =
                                    board
                                        |> List.map
                                            (\c ->
                                                if c.position == head then
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
                                if c.position == currentPositionToClean then
                                    { c | value = Empty }

                                else
                                    c
                            )

                onlyOneSolution =
                    hasOnlyOneSolution updatedBoard <| getPositionsOfFreeCells updatedBoard
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
                getBoxIndex pos == getBoxIndex c.position
            )
        |> List.filter
            (\c ->
                case skipCell of
                    Just v ->
                        v /= c.position

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
                        c.position
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
                        c.position
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


setComplexity : Complexity -> Model
setComplexity complexity =
    { emptyModel | complexity = complexity }


rawIndexToPossiblePosition : Int -> ( Maybe Index, Maybe Index )
rawIndexToPossiblePosition i =
    let
        row =
            i // size

        column =
            remainderBy size i
    in
    ( fromInt row, fromInt column )


getPositionsOfFreeCells : Board -> List Position
getPositionsOfFreeCells board =
    board
        |> List.filter (\c -> c.value == Empty)
        |> List.map (\c -> c.position)


isValidValue : Board -> ( Position, CValue ) -> Bool
isValidValue board ( position, cvalue ) =
    let
        ( row, col ) =
            position
    in
    (getAllNonEmptyValuesWithSkip (Just col) True board row |> List.member cvalue |> not)
        && (getAllNonEmptyValuesWithSkip (Just row) False board col |> List.member cvalue |> not)
        && (getAllNonEmptyValuesInBoxWithSkip (Just position) board position |> List.member cvalue |> not)


emptyModel : Model
emptyModel =
    Model initEmptyBoard initEmptyBoard [] [] Easy Nothing (Just "Generating board")


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
        |> List.map (\pos -> BoardCell pos Empty Initial)
