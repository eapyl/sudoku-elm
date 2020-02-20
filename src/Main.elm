module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Json.Decode exposing (Value)
import Model exposing (BoxGroup(..), CValue(..), Index(..), Model)
import Msg exposing (Msg(..))
import Sudoku exposing (emptyModel)
import Update exposing (initCommand, update)
import View exposing (view)


main : Program Value Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( emptyModel
                , initCommand
                )
        , update = update
        , view =
            \model ->
                { title = "Sudoku"
                , body = List.map toUnstyled <| view model
                }
        , subscriptions = \_ -> Sub.none
        }
