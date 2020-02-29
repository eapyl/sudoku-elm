module Sudoku.View exposing (view)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Sudoku.Model exposing (Board, CValue(..), Cell, Index, ModalCValue(..), Model, Position)
import Sudoku.Msg exposing (Msg(..))
import Sudoku.Update exposing (isValidValue)
import Sudoku.Utils exposing (allIndexes, indexToInt, toString)


view : Model -> Element Msg
view model =
    let
        currentBoard =
            model.board
    in
    case model.status of
        Just txt ->
            el [ centerX, centerY ]
                (text txt)

        _ ->
            column
                [ width fill, height fill ]
                (allIndexes
                    |> List.map
                        (\v -> generateRow model.freeCells model.selectedCell currentBoard v)
                )


generateRow : List Position -> Maybe Position -> Board -> Index -> Element Msg
generateRow initialFreeCells selected board index =
    row
        [ width fill
        , height fill
        ]
        (allIndexes
            |> List.filterMap (\col -> getCell board ( index, col ))
            |> List.map
                (\cell ->
                    let
                        ( row, col ) =
                            cell.pos

                        isGray =
                            let
                                evenRow =
                                    indexToInt row // 3 == 1

                                evenCol =
                                    indexToInt col // 3 == 1
                            in
                            (not evenRow && evenCol) || (evenRow && not evenCol)

                        hasModal =
                            List.member cell.pos initialFreeCells

                        modalColor =
                            if hasModal then
                                if isValidValue board cell then
                                    Just green600

                                else
                                    Just red600

                            else
                                Nothing
                    in
                    case modalCell selected cell.pos of
                        Just modalCellValue ->
                            modalCellElement modalCellValue

                        Nothing ->
                            if isGray then
                                grayCell modalColor cell

                            else
                                normalCell modalColor cell
                )
        )


getCell : Board -> Position -> Maybe (Cell CValue)
getCell board ( row, column ) =
    board
        |> List.filter
            (\c ->
                let
                    ( r, col ) =
                        c.pos
                in
                r == row && col == column
            )
        |> List.head


modalCell : Maybe Position -> Position -> Maybe (Cell ModalCValue)
modalCell selectedCell ( row, col ) =
    let
        targetRow =
            indexToInt row

        targetCol =
            indexToInt col

        selectedToModalCell ( selectedRow, selectedCol ) =
            let
                intRow =
                    indexToInt selectedRow

                intCol =
                    indexToInt selectedCol

                targetPosition =
                    ( selectedRow, selectedCol )

                startRow =
                    if intRow // 2 >= 3 then
                        intRow - 3

                    else
                        intRow

                startCol =
                    if intCol // 2 >= 3 then
                        intCol - 3

                    else
                        intCol + 1
            in
            if targetRow == startRow && targetCol == startCol then
                Just <| Cell targetPosition (Number One)

            else if targetRow == startRow && targetCol == startCol + 1 then
                Just <| Cell targetPosition (Number Two)

            else if targetRow == startRow && targetCol == startCol + 2 then
                Just <| Cell targetPosition (Number Three)

            else if targetRow == startRow + 1 && targetCol == startCol then
                Just <| Cell targetPosition (Number Four)

            else if targetRow == startRow + 1 && targetCol == startCol + 1 then
                Just <| Cell targetPosition (Number Five)

            else if targetRow == startRow + 1 && targetCol == startCol + 2 then
                Just <| Cell targetPosition (Number Six)

            else if targetRow == startRow + 2 && targetCol == startCol then
                Just <| Cell targetPosition (Number Seven)

            else if targetRow == startRow + 2 && targetCol == startCol + 1 then
                Just <| Cell targetPosition (Number Eight)

            else if targetRow == startRow + 2 && targetCol == startCol + 2 then
                Just <| Cell targetPosition (Number Nine)

            else if targetRow == startRow + 3 && targetCol == startCol then
                Just <| Cell targetPosition (Number Empty)

            else if targetRow == startRow + 3 && targetCol == startCol + 1 then
                Just <| Cell targetPosition EmptyValue

            else if targetRow == startRow + 3 && targetCol == startCol + 2 then
                Just <| Cell targetPosition Back

            else
                Nothing
    in
    Maybe.andThen selectedToModalCell selectedCell


modalCellElement : Cell ModalCValue -> Element Msg
modalCellElement modalCellValue =
    let
        txt =
            case modalCellValue.value of
                Number Empty ->
                    "∅"

                Number cvalue ->
                    toString cvalue

                EmptyValue ->
                    " "

                Back ->
                    "←"

        innerElement =
            el
                [ Font.center
                , centerY
                , centerX
                , Font.size 30
                ]
                (text txt)
    in
    case modalCellValue.value of
        EmptyValue ->
            el
                modalStyle
                innerElement

        _ ->
            Input.button
                modalStyle
                { onPress = Just <| ModalCommand modalCellValue
                , label = innerElement
                }


grayCell : Maybe Color -> Cell CValue -> Element Msg
grayCell =
    boardCell gray400


normalCell : Maybe Color -> Cell CValue -> Element Msg
normalCell =
    boardCell white


modalStyle : List (Attribute Msg)
modalStyle =
    mixin fillAttr
        [ Background.color gray200
        , Border.color gray200
        ]


boardCell : Color -> Maybe Color -> Cell CValue -> Element Msg
boardCell color modalColor cell =
    let
        baseAttr =
            mixin fillAttr
                [ Background.color <| color ]

        innerElement =
            el
                [ Font.center
                , centerY
                , centerX
                , Font.size 30
                ]
                (text <| toString cell.value)
    in
    case modalColor of
        Just mC ->
            Input.button
                (mixin baseAttr
                    [ Font.italic
                    , Font.color mC
                    ]
                )
                { onPress = Just <| ShowModalWindow cell.pos
                , label = innerElement
                }

        Nothing ->
            el
                baseAttr
                innerElement


mixin : List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
mixin base new =
    List.append base new


fillAttr : List (Attribute Msg)
fillAttr =
    border
        [ width fill
        , height fill
        ]


border : List (Attribute msg) -> List (Attribute msg)
border =
    mixin
        [ Border.solid
        , Border.color (rgb 0 0 0)
        , Border.width 1
        ]
