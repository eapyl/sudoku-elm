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
import Sudoku
    exposing
        ( Board
        , CValue(..)
        , Cell
        , Complexity(..)
        , Index(..)
        , Position
        , allIndexes
        , freeCellPositionsOnBoard
        , fromInt
        , getCell
        , indexToInt
        , isValidValue
        , toString
        )


view : Model -> List (Html Msg)
view model =
    [ layoutWith { options = [ focusStyle <| FocusStyle Nothing Nothing Nothing ] } [] (mainView model)
    ]


mainView : Model -> Element Msg
mainView model =
    let
        currentBoard =
            model.sudoku.board

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
            [ case model.sudoku.status of
                Just txt ->
                    el [ centerX, centerY ]
                        (text txt)

                _ ->
                    column
                        [ width fill, height fill ]
                        (allIndexes
                            |> List.map
                                (\v -> generateRow (freeCellPositionsOnBoard model.sudoku) model.selectedCell currentBoard v)
                        )
            ]
        , row [ spacing 5, paddingXY 0 5, width fill ]
            [ createButton Easy
            , createButton Normal
            , createButton Hard
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


modalPosition : Position -> Maybe ( Position, Position )
modalPosition ( row, col ) =
    let
        intRow =
            indexToInt row

        intCol =
            indexToInt col

        rightSide =
            intCol // 2 >= 3

        bottomSide =
            intRow // 2 >= 3

        leftTopCorner =
            case ( rightSide, bottomSide ) of
                ( True, True ) ->
                    ( fromInt (intRow - 3), fromInt (intCol - 3) )

                ( True, False ) ->
                    ( fromInt intRow, fromInt (intCol - 3) )

                ( False, True ) ->
                    ( fromInt (intRow - 3), fromInt (intCol + 1) )

                ( False, False ) ->
                    ( fromInt intRow, fromInt (intCol + 1) )

        rightBotCorner =
            case ( rightSide, bottomSide ) of
                ( True, True ) ->
                    ( fromInt intRow, fromInt (intCol - 1) )

                ( True, False ) ->
                    ( fromInt (intRow + 3), fromInt (intCol - 1) )

                ( False, True ) ->
                    ( fromInt intRow, fromInt (intCol + 3) )

                ( False, False ) ->
                    ( fromInt (intRow + 3), fromInt (intCol + 3) )
    in
    case ( leftTopCorner, rightBotCorner ) of
        ( ( Just r1, Just c1 ), ( Just r2, Just c2 ) ) ->
            Just ( ( r1, c1 ), ( r2, c2 ) )

        _ ->
            Nothing


type ModalCommand
    = SelCValue CValue
    | Close
    | NoModal


valueInModal : Position -> ( Position, Position ) -> Maybe ModalCommand
valueInModal ( r, c ) ( ( r1, c1 ), _ ) =
    let
        intR =
            indexToInt r

        intC =
            indexToInt c

        intR1 =
            indexToInt r1

        intC1 =
            indexToInt c1
    in
    if intR == intR1 then
        if intC == intC1 then
            Just <| SelCValue One

        else if intC == intC1 + 1 then
            Just <| SelCValue Two

        else if intC == intC1 + 2 then
            Just <| SelCValue Three

        else
            Nothing

    else if intR == intR1 + 1 then
        if intC == intC1 then
            Just <| SelCValue Four

        else if intC == intC1 + 1 then
            Just <| SelCValue Five

        else if intC == intC1 + 2 then
            Just <| SelCValue Six

        else
            Nothing

    else if intR == intR1 + 2 then
        if intC == intC1 then
            Just <| SelCValue Seven

        else if intC == intC1 + 1 then
            Just <| SelCValue Eight

        else if intC == intC1 + 2 then
            Just <| SelCValue Nine

        else
            Nothing

    else if intR == intR1 + 3 then
        if intC == intC1 then
            Just <| SelCValue Empty

        else if intC == intC1 + 1 then
            Just <| NoModal

        else if intC == intC1 + 2 then
            Just <| Close

        else
            Nothing

    else
        Nothing


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

                        modalCommand =
                            Maybe.andThen modalPosition selected |> Maybe.andThen (valueInModal cell.pos)

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
                    case ( modalCommand, selected ) of
                        ( Just command, Just posToWrite ) ->
                            modalCell posToWrite command

                        _ ->
                            if isGray then
                                grayCell modalColor cell

                            else
                                normalCell modalColor cell
                )
        )


fillAttr : List (Attribute Msg)
fillAttr =
    border
        [ width fill
        , height fill
        ]


boardCell : Color -> Maybe Color -> Cell -> Element Msg
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
                { onPress = Just <| ShowModal cell.pos
                , label = innerElement
                }

        Nothing ->
            el
                baseAttr
                innerElement


grayCell : Maybe Color -> Cell -> Element Msg
grayCell =
    boardCell gray400


normalCell : Maybe Color -> Cell -> Element Msg
normalCell =
    boardCell white


modalStyle : List (Attribute Msg)
modalStyle =
    mixin fillAttr
        [ Background.color gray200
        , Border.color gray200
        ]


modalCell : Position -> ModalCommand -> Element Msg
modalCell pos command =
    let
        txt =
            case command of
                SelCValue Empty ->
                    "∅"

                SelCValue cvalue ->
                    toString cvalue

                NoModal ->
                    ""

                Close ->
                    "←"

        cmn =
            case command of
                Close ->
                    Just CloseModal

                SelCValue cvalue ->
                    Just <| SelectedCValue pos cvalue

                _ ->
                    Nothing

        innerElement =
            el
                [ Font.center
                , centerY
                , centerX
                , Font.size 30
                ]
                (text txt)
    in
    case command of
        NoModal ->
            el
                modalStyle
                innerElement

        _ ->
            Input.button
                modalStyle
                { onPress = cmn
                , label = innerElement
                }
