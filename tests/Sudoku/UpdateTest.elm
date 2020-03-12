module Sudoku.UpdateTest exposing (updateTest)

import Expect
import Fuzz
import Random
import Shrink
import Sudoku.Model exposing (Board, BoardCell, BoardCellType(..), Index, Position, Value(..), boardSize, size)
import Sudoku.Msg exposing (..)
import Sudoku.Update exposing (emptyModel, update)
import Sudoku.Utils exposing (allValues, fromInt)
import Test exposing (Test, describe, fuzz, test, todo)


randomValueListFuzzer : Fuzz.Fuzzer (List Value)
randomValueListFuzzer =
    Fuzz.map (List.repeat boardSize) (Fuzz.custom (Random.uniform Empty allValues) Shrink.noShrink)


updateTest : Test
updateTest =
    describe "Main"
        [ describe "Update tests"
            [ fuzz randomValueListFuzzer "InitialValuesForBoardGenerated - updated board and next command called" <|
                \randomBoardValues ->
                    update (InitialValuesForBoardGenerated randomBoardValues) emptyModel
                        |> Expect.all
                            [ \( m, _ ) ->
                                if List.any (\a -> a == Empty) randomBoardValues then
                                    Expect.equal Nothing m.status

                                else
                                    Expect.equal (Just "No empty cells on board") m.status
                            , \( m, _ ) -> Expect.equalLists (List.map (\x -> x.value) m.board) randomBoardValues
                            ]
            , todo "FreeCellSelected"
            , todo "RemoveValueFromBoard"
            , todo "RandomValueGenerated"
            , todo "DelayCommand"
            , todo "ShowModalWindow"
            , todo "ModalCommand"
            ]
        , todo "Create board"
        , todo "Get empty board"
        , todo "Set complexity"
        ]
