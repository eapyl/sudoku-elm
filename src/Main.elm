module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import CssStyles exposing (btnCss, divBottomCss, divInBtnCss, divInDivCss, divInTdCss, mainDivCss, tableCss, tdCss)
import Html.Styled exposing (Html, button, div, td, text, toUnstyled, tr)
import Html.Styled.Events exposing (onClick)
import Json.Decode exposing (Value)
import Random
import Random.List



--
-- MODEL
--


type CValue
    = Empty
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type alias Position =
    ( Index, Index )


type alias Cell =
    { pos : Position
    , value : CValue
    }


type Index
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Eighth
    | Ninth


type alias Board =
    Array Cell


type alias Model =
    { board : Board
    , triedValues : List ( Position, CValue )
    , generationStatus : Maybe String
    }



--
-- MSG
--


type Msg
    = GenerateBoard
    | FreeCellSelected (List Cell) Cell
    | RandomValueGenerated (List Cell) Position CValue
    | NextGo
    | ValuesForBoxGenerated ( BoxGroup, BoxGroup ) (Maybe ( BoxGroup, BoxGroup )) (List CValue)



--
-- UPDATE
--


allValues : List CValue
allValues =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]


valueCompleteGenerator : Random.Generator (List CValue)
valueCompleteGenerator =
    Random.List.shuffle [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]


valueGenerator : CValue -> List CValue -> Random.Generator CValue
valueGenerator initial rest =
    Random.uniform initial rest


freeCellGenerator : Cell -> List Cell -> Random.Generator Cell
freeCellGenerator initialCell otherFreeCells =
    Random.uniform initialCell otherFreeCells


generateBoard : List Cell -> Cmd Msg
generateBoard freeCells =
    case freeCells of
        head :: tail ->
            Random.generate (FreeCellSelected freeCells) <| freeCellGenerator head tail

        [] ->
            Cmd.none


type SolutionCount
    = Zero
    | Single
    | More


runBacktracking : Board -> List Cell -> SolutionCount
runBacktracking =
    backtracking Zero


backtracking : SolutionCount -> Board -> List Cell -> SolutionCount
backtracking count board freeCells =
    case freeCells of
        head :: tail ->
            let
                usedValues =
                    getUsedValues board head.pos

                tryValuesForCell solutionCount possibleValues =
                    case possibleValues of
                        possibleValue :: otherValues ->
                            let
                                updatedBoard =
                                    board
                                        |> Array.map
                                            (\c ->
                                                if c.pos == head.pos then
                                                    { c | value = possibleValue }

                                                else
                                                    c
                                            )
                            in
                            case backtracking solutionCount updatedBoard tail of
                                Zero ->
                                    tryValuesForCell Zero otherValues

                                Single ->
                                    Single

                                More ->
                                    More

                        [] ->
                            Zero
            in
            allValues
                |> List.filter (\a -> List.member a usedValues |> not)
                |> tryValuesForCell count

        [] ->
            case count of
                Zero ->
                    Single

                Single ->
                    More

                More ->
                    More


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBoard ->
            ( model
            , generateBoard <| getFreeCells model.board
            )

        NextGo ->
            ( model
            , generateBoard <| getFreeCells model.board
            )

        RandomValueGenerated freeCells cellPos randomValue ->
            let
                filteredCells =
                    freeCells
                        |> List.filter (\c -> c.pos /= cellPos)

                updatedBoard =
                    model.board
                        |> Array.map
                            (\c ->
                                if c.pos == cellPos then
                                    { c | value = randomValue }

                                else
                                    c
                            )

                tryToSolve =
                    runBacktracking updatedBoard filteredCells

                ( updatedModel, remainingCells ) =
                    if List.member ( cellPos, randomValue ) model.triedValues then
                        ( model.board, freeCells )

                    else
                        case tryToSolve of
                            Zero ->
                                ( model.board, freeCells )

                            _ ->
                                ( updatedBoard, filteredCells )
            in
            ( { model
                | board = updatedModel
                , triedValues = ( cellPos, randomValue ) :: model.triedValues
                , generationStatus =
                    if List.isEmpty remainingCells then
                        Nothing

                    else
                        Just <| (((54 - List.length freeCells) * 100 // 54) |> String.fromInt) ++ "%"
              }
            , generateBoard remainingCells
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
                        |> Array.map
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
            ( { model | board = updatedBoard }
            , case nextBoxPosition of
                Just ( B, B ) ->
                    Random.generate (ValuesForBoxGenerated ( B, B ) (Just ( C, C ))) valueCompleteGenerator

                Just ( C, C ) ->
                    Random.generate (ValuesForBoxGenerated ( C, C ) Nothing) valueCompleteGenerator

                _ ->
                    generateBoard <| getFreeCells updatedBoard
            )



--
-- SUDOKU
--


getUsedValues : Board -> Position -> List CValue
getUsedValues board ( row, col ) =
    let
        inBox =
            getAllNonEmptyValuesInBox board ( row, col )

        inRow =
            getAllNonEmptyValuesInRow board row
                |> List.filter (\a -> List.member a inBox |> not)

        inCol =
            getAllNonEmptyValuesInColumn board col
                |> List.filter (\a -> List.member a inBox |> not)
                |> List.filter (\a -> List.member a inRow |> not)
    in
    List.append inBox inRow
        |> List.append inCol


type BoxGroup
    = A
    | B
    | C


getBoxIndex : Position -> Index
getBoxIndex ( row, col ) =
    let
        boxRowColumn ind =
            if ind == First || ind == Second || ind == Third then
                A

            else if ind == Fourth || ind == Fifth || ind == Sixth then
                B

            else
                C
    in
    case ( boxRowColumn row, boxRowColumn col ) of
        ( A, A ) ->
            First

        ( A, B ) ->
            Second

        ( A, C ) ->
            Third

        ( B, A ) ->
            Fourth

        ( B, B ) ->
            Fifth

        ( B, C ) ->
            Sixth

        ( C, A ) ->
            Seventh

        ( C, B ) ->
            Eighth

        ( C, C ) ->
            Ninth


getAllNonEmptyValuesInBox : Board -> Position -> List CValue
getAllNonEmptyValuesInBox board pos =
    board
        |> Array.filter (\c -> c.value /= Empty)
        |> Array.filter
            (\c ->
                getBoxIndex pos == getBoxIndex c.pos
            )
        |> Array.map (\c -> c.value)
        |> Array.toList


getAllNonEmptyValuesInRow : Board -> Index -> List CValue
getAllNonEmptyValuesInRow =
    getAllNonEmptyValues True


getAllNonEmptyValuesInColumn : Board -> Index -> List CValue
getAllNonEmptyValuesInColumn =
    getAllNonEmptyValues False


getAllNonEmptyValues : Bool -> Board -> Index -> List CValue
getAllNonEmptyValues isRow board index =
    board
        |> Array.filter (\c -> c.value /= Empty)
        |> Array.filter
            (\c ->
                let
                    ( row, col ) =
                        c.pos
                in
                if isRow then
                    row == index

                else
                    col == index
            )
        |> Array.map (\c -> c.value)
        |> Array.toList



--
-- UTILS
--


fromInt : Int -> Maybe Index
fromInt index =
    case index of
        0 ->
            Just First

        1 ->
            Just Second

        2 ->
            Just Third

        3 ->
            Just Fourth

        4 ->
            Just Fifth

        5 ->
            Just Sixth

        6 ->
            Just Seventh

        7 ->
            Just Eighth

        8 ->
            Just Ninth

        _ ->
            Nothing


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


size : Int
size =
    9


boardSize : Int
boardSize =
    size * size


rawIndexToPossiblePosition : Int -> ( Maybe Index, Maybe Index )
rawIndexToPossiblePosition i =
    let
        row =
            i // size

        column =
            remainderBy size i
    in
    ( fromInt row, fromInt column )


initEmptyBoard : Board
initEmptyBoard =
    Array.initialize boardSize (\i -> rawIndexToPossiblePosition i)
        |> Array.toList
        |> List.filterMap
            (\( r, c ) ->
                case ( r, c ) of
                    ( Just rV, Just cV ) ->
                        Just ( rV, cV )

                    _ ->
                        Nothing
            )
        |> List.map (\pos -> Cell pos Empty)
        |> Array.fromList


getCValue : Board -> Position -> CValue
getCValue board ( row, column ) =
    board
        |> Array.filter
            (\c ->
                let
                    ( r, col ) =
                        c.pos
                in
                r == row && col == column
            )
        |> Array.get 0
        |> Maybe.map (\c -> c.value)
        |> Maybe.withDefault Empty


getFreeCells : Board -> List Cell
getFreeCells board =
    board
        |> Array.filter (\c -> c.value == Empty)
        |> Array.toList



--
-- VIEW
--


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
            , div [] [ button [ onClick NextGo ] [ text "Next" ] ]
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
    Array.initialize size fromInt
        |> Array.toList
        |> List.filterMap (\i -> i)
        |> List.map (\col -> getCValue board ( index, col ))
        |> List.map generateTd



--
-- MAIN
--


main : Program Value Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( Model initEmptyBoard [] (Just "Generating")
                , Random.generate (ValuesForBoxGenerated ( A, A ) (Just ( B, B ))) valueCompleteGenerator
                )
        , update = update
        , view =
            \model ->
                { title = "Sudoku"
                , body = List.map toUnstyled <| view model
                }
        , subscriptions = \_ -> Sub.none
        }
