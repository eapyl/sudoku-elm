module Sudoku.Update exposing (createBoard, emptyModel, setComplexity, update)

import Process
import Random
import Random.List
import Sudoku.Model
    exposing
        ( Board
        , BoardCell
        , BoardCellType(..)
        , Complexity(..)
        , Index
        , ModalCell
        , ModalValue(..)
        , Model
        , Position
        , Value(..)
        , boardSize
        , size
        )
import Sudoku.Msg exposing (Msg(..))
import Sudoku.Utils exposing (allValues, fromInt, indexToInt)
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialValuesForBoardGenerated values ->
            let
                updatedBoard =
                    values
                        |> List.map2
                            (\boardCell ->
                                \generatedValue ->
                                    { boardCell | value = generatedValue }
                            )
                            model.board
            in
            ( { model | board = updatedBoard, status = Nothing }
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
                            isValidValue model.board ( cell.position, selectedCValue )

                        newBoard =
                            if isValid then
                                updateCellOnBoard model.board (BoardCell cell.position selectedCValue Valid)

                            else
                                updateCellOnBoard model.board (BoardCell cell.position selectedCValue Invalid)
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


valueGenerator : Value -> List Value -> Random.Generator Value
valueGenerator initial rest =
    Random.uniform initial rest


createBoard : Cmd Msg
createBoard =
    Random.generate InitialValuesForBoardGenerated initialValuesForBoardGenerator


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


initialValuesForBoardGenerator : Random.Generator (List Value)
initialValuesForBoardGenerator =
    let
        listGenerator col =
            Random.List.shuffle allValues
                |> Random.andThen
                    (\list ->
                        let
                            rowForList =
                                createRowGenerator list
                        in
                        (rowForList 0 col
                            ++ rowForList 1 col
                            ++ rowForList 2 col
                        )
                            |> Random.constant
                    )

        createRowGenerator list row col =
            if col == 1 then
                (list |> List.drop (3 * row) |> List.take 3) ++ List.repeat 6 Empty

            else if col == 2 then
                List.repeat 3 Empty ++ (list |> List.drop (3 * row) |> List.take 3) ++ List.repeat 3 Empty

            else
                List.repeat 6 Empty ++ (list |> List.drop (3 * row) |> List.take 3)
    in
    Random.map3
        (\a -> \b -> \c -> a ++ b ++ c)
        (listGenerator 1)
        (listGenerator 2)
        (listGenerator 3)


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


getUsedValues : Board -> Position -> List Value
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


getAllNonEmptyValuesInBox : Board -> Position -> List Value
getAllNonEmptyValuesInBox =
    getAllNonEmptyValuesInBoxWithSkip Nothing


getAllNonEmptyValuesInBoxWithSkip : Maybe Position -> Board -> Position -> List Value
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


getAllNonEmptyValuesInRow : Board -> Index -> List Value
getAllNonEmptyValuesInRow =
    getAllNonEmptyValues True


getAllNonEmptyValuesInColumn : Board -> Index -> List Value
getAllNonEmptyValuesInColumn =
    getAllNonEmptyValues False


getAllNonEmptyValues : Bool -> Board -> Index -> List Value
getAllNonEmptyValues =
    getAllNonEmptyValuesWithSkip Nothing


getAllNonEmptyValuesWithSkip : Maybe Index -> Bool -> Board -> Index -> List Value
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


isValidValue : Board -> ( Position, Value ) -> Bool
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
