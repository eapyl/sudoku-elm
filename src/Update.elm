module Update exposing (initCommand, update)

import Convert exposing (fromInt)
import Model exposing (BoxGroup(..), CValue(..), Cell, Level(..), Model, Position)
import Msg exposing (Msg(..))
import Process
import Random
import Random.List
import Sudoku
    exposing
        ( allValues
        , boardSize
        , emptyModel
        , getFreeCells
        , getUsedValues
        , hasAtLeastOneSolution
        , rawIndexToPossiblePosition
        , tryToRemoveValuesFromBoard
        )
import Task


sendDelayed : (a -> msg) -> a -> Cmd msg
sendDelayed msg a =
    Process.sleep 20
        |> Task.perform (\_ -> msg a)


initCommand : Cmd Msg
initCommand =
    Random.generate (ValuesForBoxGenerated ( A, A ) (Just ( B, B ))) valueCompleteGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBoard ->
            ( model
            , sendDelayed DelayCommand <| getFreeCells model.board
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
                , message =
                    if List.isEmpty remainingCells then
                        Nothing

                    else
                        Just <| (((54 - List.length freeCells) * 100 // 54) |> String.fromInt) ++ "%"
              }
            , sendDelayed DelayCommand remainingCells
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

        RemoveValueFromBoard positionsToClean ->
            let
                mapLevelToInt =
                    case model.level of
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
                , initialFreeCells = freeCellsPositions
                , generated = True
              }
            , Cmd.none
            )

        ShowModal pos ->
            ( { model | selectedCell = Just pos }, Cmd.none )

        CloseModal ->
            ( { model | selectedCell = Nothing }, Cmd.none )

        SelectedCValue pos cvalue ->
            let
                updatedBoard =
                    model.board
                        |> List.map
                            (\c ->
                                if c.pos == pos then
                                    { c | value = cvalue }

                                else
                                    c
                            )
            in
            if model.solution == updatedBoard then
                ( { model
                    | selectedCell = Nothing
                    , board = updatedBoard
                    , message = Just "Solved!"
                  }
                , Cmd.none
                )

            else
                ( { model | selectedCell = Nothing, board = updatedBoard }, Cmd.none )

        ChangeLevel newLevel ->
            ( { emptyModel | level = newLevel, message = Just "Start generating board" }, initCommand )

        DelayCommand remainingCells ->
            ( model
            , generateBoard remainingCells
            )


generateBoard : List Cell -> Cmd Msg
generateBoard freeCells =
    case freeCells of
        head :: tail ->
            Random.generate (FreeCellSelected freeCells) <| freeCellGenerator head tail

        [] ->
            Random.generate RemoveValueFromBoard positionCompleteGenerator


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


valueCompleteGenerator : Random.Generator (List CValue)
valueCompleteGenerator =
    Random.List.shuffle allValues


valueGenerator : CValue -> List CValue -> Random.Generator CValue
valueGenerator initial rest =
    Random.uniform initial rest


freeCellGenerator : Cell -> List Cell -> Random.Generator Cell
freeCellGenerator initialCell otherFreeCells =
    Random.uniform initialCell otherFreeCells
