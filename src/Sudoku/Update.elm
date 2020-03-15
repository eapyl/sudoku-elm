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
        , ModalValue(..)
        , Model
        , Position
        , Value(..)
        , boardSize
        , size
        )
import Sudoku.Msg exposing (Msg(..))
import Sudoku.Utils exposing (allValues, fromInt)
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

                freeCellPositions =
                    getPositionsOfFreeCells updatedBoard
            in
            ( { model | board = updatedBoard, status = Just <| "Initial board generated." }
            , Random.generate (FreeCellSelected freeCellPositions) <| cellGenerator updatedBoard freeCellPositions
            )

        FreeCellSelected freeCellPositions boardCell ->
            case boardCell of
                Just cell ->
                    let
                        filteredCells =
                            freeCellPositions
                                |> List.filter (\c -> c /= cell.position)

                        updatedBoard =
                            updateCellOnBoard model.board cell

                        ( updatedModel, remainingCells ) =
                            if hasAtLeastOneSolution updatedBoard filteredCells then
                                ( updatedBoard, filteredCells )

                            else
                                ( model.board, freeCellPositions )

                        freeCellNumberAfterInitialization =
                            boardSize - 3 * size

                        freeCellLeft =
                            freeCellNumberAfterInitialization - List.length freeCellPositions

                        percent =
                            freeCellLeft * 100 // freeCellNumberAfterInitialization
                    in
                    ( { model
                        | solution = updatedModel
                        , board = updatedModel
                        , status =
                            if List.isEmpty remainingCells then
                                Just "Finalizing"

                            else
                                Just <| (percent |> String.fromInt) ++ "%"
                      }
                    , sendDelayed DelayCommand remainingCells
                    )

                Nothing ->
                    ( model
                    , Random.generate RemoveValueFromBoard positionCompleteGenerator
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

        DelayCommand remainingCells ->
            ( model
            , Random.generate (FreeCellSelected remainingCells) <| cellGenerator model.board remainingCells
            )

        ShowModalWindow selectedPos ->
            ( { model | selectedCell = Just selectedPos }
            , Cmd.none
            )

        ModalCommand cell ->
            case cell.value of
                Number selectedValue ->
                    let
                        isValid =
                            isValidValue model.board ( cell.position, selectedValue )

                        newBoard =
                            if isValid then
                                updateCellOnBoard model.board (BoardCell cell.position selectedValue Valid)

                            else
                                updateCellOnBoard model.board (BoardCell cell.position selectedValue Invalid)
                    in
                    ( { model
                        | board = newBoard
                        , selectedCell = Nothing
                        , status =
                            if List.any (\a -> a.category == Invalid) newBoard then
                                Nothing

                            else
                                Just "Solved!"
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
    Process.sleep 0
        |> Task.perform (\_ -> msg a)


createBoard : Cmd Msg
createBoard =
    Random.generate InitialValuesForBoardGenerated initialValuesForBoardGenerator


cellGenerator : Board -> List Position -> Random.Generator (Maybe BoardCell)
cellGenerator board freeCellPositions =
    let
        boardCellGenerator : Position -> Random.Generator (Maybe BoardCell)
        boardCellGenerator freeCellPosition =
            let
                usedValues =
                    getUsedValues board freeCellPosition

                getPossibleValuesForCell =
                    allValues
                        |> List.filter (\a -> List.member a usedValues |> not)
            in
            case getPossibleValuesForCell of
                h :: _ ->
                    Random.uniform h getPossibleValuesForCell
                        |> Random.map
                            (\value ->
                                Just <| BoardCell freeCellPosition value Initial
                            )

                [] ->
                    Random.constant Nothing
    in
    case freeCellPositions of
        head :: _ ->
            Random.uniform head freeCellPositions
                |> Random.andThen boardCellGenerator

        [] ->
            Random.constant Nothing


positionCompleteGenerator : Random.Generator (List Position)
positionCompleteGenerator =
    List.range 0 (boardSize - 1)
        |> List.map rawIndexToPossiblePosition
        |> List.filterMap
            (\( mr, mc, mb ) ->
                case ( mr, mc, mb ) of
                    ( Just row, Just col, Just box ) ->
                        Just <| Position row col box

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
                    cell

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
                                    { c | value = Empty, category = Invalid }

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
getUsedValues board { row, col, box } =
    board
        |> List.filter
            (\c ->
                c.value
                    /= Empty
                    && (c.position.row == row || c.position.col == col || c.position.box == box)
            )
        |> List.map (\c -> c.value)


getAllNonEmptyValuesForCell : Board -> Position -> List Value
getAllNonEmptyValuesForCell board position =
    let
        { row, col, box } =
            position

        isFiltered pos =
            pos.row == row || pos.col == col || pos.box == box
    in
    board
        |> List.filter (\c -> c.position /= position && c.value /= Empty && isFiltered c.position)
        |> List.map (\c -> c.value)


setComplexity : Complexity -> Model
setComplexity complexity =
    { emptyModel | complexity = complexity }


rawIndexToPossiblePosition : Int -> ( Maybe Index, Maybe Index, Maybe Index )
rawIndexToPossiblePosition i =
    let
        row =
            i // size

        column =
            remainderBy size i

        box =
            row // 3 * 3 + column // 3
    in
    ( fromInt row, fromInt column, fromInt box )


getPositionsOfFreeCells : Board -> List Position
getPositionsOfFreeCells board =
    board
        |> List.filter (\c -> c.value == Empty)
        |> List.map (\c -> c.position)


isValidValue : Board -> ( Position, Value ) -> Bool
isValidValue board ( position, cvalue ) =
    getAllNonEmptyValuesForCell board position |> List.member cvalue |> not


emptyModel : Model
emptyModel =
    Model initEmptyBoard initEmptyBoard [] Easy Nothing (Just "Generating board")


initEmptyBoard : Board
initEmptyBoard =
    List.range 0 (boardSize - 1)
        |> List.map (\i -> rawIndexToPossiblePosition i)
        |> List.filterMap
            (\( r, c, b ) ->
                case ( r, c, b ) of
                    ( Just row, Just col, Just box ) ->
                        Just <| Position row col box

                    _ ->
                        Nothing
            )
        |> List.map (\pos -> BoardCell pos Empty Initial)
