module View exposing (view)

import Html.Styled exposing (Html, button, div, td, text, tr)
import Html.Styled.Events exposing (onClick)
import Model exposing (Board, CValue(..), Index(..), Model)
import Msg exposing (Msg)
import Styles exposing (btnCss, btnRightCss, divBottomCss, divInDivCss, divInTdCss, mainDivCss, tableCss, tdCss)
import Sudoku exposing (allIndexes, getCValue, indexToInt)


view : Model -> List (Html Msg)
view model =
    [ div [ mainDivCss ]
        (mainView model)
    ]


modalWindow : List (Html Msg)
modalWindow =
    [ Html.Styled.table []
        [ tr
            []
            [ td [] [ text "sample " ]
            ]
        ]
    ]


mainView : Model -> List (Html Msg)
mainView model =
    let
        currentBoard =
            model.board
    in
    case model.generationStatus of
        Just s ->
            [ text s ]

        Nothing ->
            [ Html.Styled.table [ tableCss ]
                (allIndexes
                    |> List.map
                        (\v -> tr [] (generateRow currentBoard v))
                )
            , div [ divBottomCss ]
                [ button [ btnCss ] [ text "Easy" ]
                , button [ btnCss ] [ text "Medium" ]
                , button [ btnCss ] [ text "Hard" ]
                , button [ btnRightCss ] [ text "Help" ]
                , button [ btnRightCss ] [ text "Solution" ]
                ]
            ]


generateTd : ( ( Index, Index ), CValue ) -> Html msg
generateTd ( ( row, col ), cellValue ) =
    let
        intRow =
            indexToInt row

        intCol =
            indexToInt col

        isGray =
            let
                evenRow =
                    intRow // 3 == 1

                evenCol =
                    intCol // 3 == 1
            in
            (not evenRow && evenCol) || (evenRow && not evenCol)
    in
    td [ tdCss isGray ] [ div [ divInTdCss ] [ div [ divInDivCss ] [ text (toString cellValue) ] ] ]


generateRow : Board -> Index -> List (Html msg)
generateRow board index =
    allIndexes
        |> List.map (\col -> ( ( index, col ), getCValue board ( index, col ) ))
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
