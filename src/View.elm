module View exposing (view)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Sudoku.Model exposing (Complexity(..))
import Sudoku.View as Sudoku exposing (view)


view : Model -> List (Html Msg)
view model =
    [ layoutWith { options = [ focusStyle <| FocusStyle Nothing Nothing Nothing ] } [] (mainView model)
    ]


mainView : Model -> Element Msg
mainView model =
    let
        width450 =
            width (fill |> maximum 450)

        height450 =
            height (fill |> maximum 450)
    in
    column
        [ centerX
        , width450
        , height450
        ]
        [ column
            (roundedBorder
                [ width450
                , height450
                ]
            )
            [ Sudoku.view model.sudoku |> Element.map SudokuCommand ]
        , row [ spacing 5, paddingXY 0 5, width fill ]
            [ createButton Easy
            , createButton Normal
            , createButton Hard
            , linkSource
            , version
            ]
        ]


version : Element msg
version =
    Element.el linkAttributes <|
        text "v1.0.4"


linkSource : Element msg
linkSource =
    link linkAttributes
        { url = "https://github.com/eapyl/sudoku-elm"
        , label = text "Source"
        }


linkAttributes : List (Attribute msg)
linkAttributes =
    [ Font.color blue
    , Element.alignRight
    , Element.alignBottom
    , Font.size 12
    , Font.family
        [ Font.monospace
        ]
    ]


mixin : List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
mixin base new =
    List.append base new


border : List (Attribute msg) -> List (Attribute msg)
border =
    mixin
        [ Border.solid
        , Border.color (rgb 0 0 0)
        , Border.width 1
        ]


roundedBorder : List (Attribute msg) -> List (Attribute msg)
roundedBorder =
    mixin
        (border
            [ Border.rounded 3
            ]
        )


createButton : Complexity -> Element Msg
createButton complexity =
    let
        txt =
            case complexity of
                Easy ->
                    "Easy"

                Normal ->
                    "Normal"

                Hard ->
                    "Hard"
    in
    Input.button
        (roundedBorder
            [ Background.color gray500
            , height fill
            ]
        )
        { onPress = Just <| ChangeLevel complexity
        , label = el [ paddingXY 3 10 ] (text txt)
        }
