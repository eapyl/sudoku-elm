module Sudoku.View exposing (view)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Sudoku.Model
    exposing
        ( Board
        , BoardCell
        , BoardCellType(..)
        , Index
        , ModalCell
        , ModalValue(..)
        , Model
        , Position
        , Value(..)
        )
import Sudoku.Msg exposing (Msg(..))
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
                        { row, col, box } =
                            cell.position

                        isGray =
                            let
                                evenRow =
                                    indexToInt row // 3 == 1

                                evenCol =
                                    indexToInt col // 3 == 1
                            in
                            (not evenRow && evenCol) || (evenRow && not evenCol)

                        hasModal =
                            List.member cell.position initialFreeCells

                        modalColor =
                            if hasModal then
                                case cell.category of
                                    Initial ->
                                        Just white

                                    Valid ->
                                        Just green600

                                    Invalid ->
                                        Just red600

                            else
                                Nothing
                    in
                    case modalCell selected cell.position of
                        Just modalCellValue ->
                            modalCellElement modalCellValue

                        Nothing ->
                            if isGray then
                                grayCell modalColor cell

                            else
                                normalCell modalColor cell
                )
        )


getCell : Board -> ( Index, Index ) -> Maybe BoardCell
getCell board ( row, col ) =
    board
        |> List.filter
            (\c ->
                c.position.row == row && c.position.col == col
            )
        |> List.head


modalCell : Maybe Position -> Position -> Maybe ModalCell
modalCell selectedCell { row, col, box } =
    let
        targetRow =
            indexToInt row

        targetCol =
            indexToInt col

        selectedToModalCell targetPosition =
            let
                intRow =
                    indexToInt targetPosition.row

                intCol =
                    indexToInt targetPosition.col

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
                Just <| ModalCell targetPosition (Number One)

            else if targetRow == startRow && targetCol == startCol + 1 then
                Just <| ModalCell targetPosition (Number Two)

            else if targetRow == startRow && targetCol == startCol + 2 then
                Just <| ModalCell targetPosition (Number Three)

            else if targetRow == startRow + 1 && targetCol == startCol then
                Just <| ModalCell targetPosition (Number Four)

            else if targetRow == startRow + 1 && targetCol == startCol + 1 then
                Just <| ModalCell targetPosition (Number Five)

            else if targetRow == startRow + 1 && targetCol == startCol + 2 then
                Just <| ModalCell targetPosition (Number Six)

            else if targetRow == startRow + 2 && targetCol == startCol then
                Just <| ModalCell targetPosition (Number Seven)

            else if targetRow == startRow + 2 && targetCol == startCol + 1 then
                Just <| ModalCell targetPosition (Number Eight)

            else if targetRow == startRow + 2 && targetCol == startCol + 2 then
                Just <| ModalCell targetPosition (Number Nine)

            else if targetRow == startRow + 3 && targetCol == startCol then
                Just <| ModalCell targetPosition (Number Empty)

            else if targetRow == startRow + 3 && targetCol == startCol + 1 then
                Just <| ModalCell targetPosition EmptyValue

            else if targetRow == startRow + 3 && targetCol == startCol + 2 then
                Just <| ModalCell targetPosition Back

            else
                Nothing
    in
    Maybe.andThen selectedToModalCell selectedCell


modalCellElement : ModalCell -> Element Msg
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


grayCell : Maybe Color -> BoardCell -> Element Msg
grayCell =
    boardCell gray400


normalCell : Maybe Color -> BoardCell -> Element Msg
normalCell =
    boardCell white


modalStyle : List (Attribute Msg)
modalStyle =
    mixin fillAttr
        [ Background.color gray200
        , Border.color gray200
        ]


boardCell : Color -> Maybe Color -> BoardCell -> Element Msg
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
                { onPress = Just <| ShowModalWindow cell.position
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
