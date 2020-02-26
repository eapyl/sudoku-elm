module Update exposing (update)

import Model exposing (Model, emptyModel)
import Msg exposing (Msg(..))
import Sudoku
    exposing
        ( Cell
        , createBoard
        , setCell
        , setComplexity
        , update
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SudokuCommand subMsg ->
            Sudoku.update subMsg model.sudoku
                |> updateWith (\subModel -> { model | sudoku = subModel }) SudokuCommand

        ShowModal pos ->
            ( { model | selectedCell = Just pos }, Cmd.none )

        CloseModal ->
            ( { model | selectedCell = Nothing }, Cmd.none )

        SelectedCValue pos cvalue ->
            let
                updatedSudoku =
                    setCell model.sudoku (Cell pos cvalue)
            in
            ( { model
                | selectedCell = Nothing
                , sudoku = updatedSudoku
              }
            , Cmd.none
            )

        ChangeLevel newLevel ->
            let
                updatedSudoku =
                    setComplexity model.sudoku newLevel
            in
            ( { emptyModel | sudoku = updatedSudoku }
            , Cmd.map SudokuCommand createBoard
            )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
