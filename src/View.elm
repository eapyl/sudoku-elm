module View exposing (view)

import Html.Styled exposing (Html, button, div, td, text, tr)
import Html.Styled.Events exposing (onClick)
import Model exposing (Board, CValue(..), Index(..), Model)
import Msg exposing (Msg)
import Styles exposing (btnCss, divBottomCss, divInBtnCss, divInDivCss, divInTdCss, mainDivCss, tableCss, tdCss)
import Sudoku exposing (fromInt, getCValue, size)


view : Model -> List (Html Msg)
view model =
    [ div [ mainDivCss ]
        (mainView model)
    ]


mainView : Model -> List (Html Msg)
mainView model =
    let
        currentBoard =
            model.board

        localGenerateRow =
            generateRow currentBoard
    in
    case model.generationStatus of
        Just s ->
            [ text s ]

        Nothing ->
            [ Html.Styled.table [ tableCss ]
                [ tr [] (localGenerateRow First)
                , tr [] (localGenerateRow Second)
                , tr [] (localGenerateRow Third)
                , tr [] (localGenerateRow Fourth)
                , tr [] (localGenerateRow Fifth)
                , tr [] (localGenerateRow Sixth)
                , tr [] (localGenerateRow Seventh)
                , tr [] (localGenerateRow Eighth)
                , tr [] (localGenerateRow Ninth)
                ]
            , div [ divBottomCss ]
                [ button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "1" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "2" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "3" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "4" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "5" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "6" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "7" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "8" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "9" ] ] ]
                , button [ btnCss ] [ div [ divInTdCss ] [ div [ divInBtnCss ] [ text "X" ] ] ]
                ]
            ]


generateTd : CValue -> Html msg
generateTd cellValue =
    td [ tdCss ] [ div [ divInTdCss ] [ div [ divInDivCss ] [ text (toString cellValue) ] ] ]


generateRow : Board -> Index -> List (Html msg)
generateRow board index =
    List.range 0 size
        |> List.map fromInt
        |> List.filterMap (\i -> i)
        |> List.map (\col -> getCValue board ( index, col ))
        |> List.map generateTd


toString : CValue -> String
toString value =
    case value of
        Empty ->
            ""

        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"
