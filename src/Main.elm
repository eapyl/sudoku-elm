module Main exposing (main)

import Browser
import Json.Decode exposing (Value)
import Model exposing (Model, emptyModel)
import Msg exposing (Msg(..))
import Sudoku exposing (createBoard)
import Update exposing (update)
import View exposing (view)


main : Program Value Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( emptyModel
                , Cmd.map SudokuCommand createBoard
                )
        , update = update
        , view =
            \model ->
                { title = "Sudoku"
                , body = view model
                }
        , subscriptions = \_ -> Sub.none
        }
