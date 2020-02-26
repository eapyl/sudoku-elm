module Update exposing (update)

import Model exposing (Level(..), Model, emptyModel)
import Msg exposing (Msg(..))
import Sudoku
    exposing
        ( createBoard
        , getFreeCells
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
                updatedSudoku sudoku updated status =
                    { sudoku | board = updated, status = status }

                updatedBoard =
                    model.sudoku.board
                        |> List.map
                            (\c ->
                                if c.pos == pos then
                                    { c | value = cvalue }

                                else
                                    c
                            )
            in
            if model.sudoku.solution == updatedBoard then
                ( { model
                    | selectedCell = Nothing
                    , sudoku = updatedSudoku model.sudoku updatedBoard (Just "Solved!")
                  }
                , Cmd.none
                )

            else
                ( { model
                    | selectedCell = Nothing
                    , sudoku = updatedSudoku model.sudoku updatedBoard Nothing
                  }
                , Cmd.none
                )

        ChangeLevel newLevel ->
            let
                updatedSudoku sudoku =
                    { sudoku | status = Just "Start generating board" }
            in
            ( { emptyModel
                | level = newLevel
                , sudoku = updatedSudoku model.sudoku
              }
            , Cmd.map SudokuCommand createBoard
            )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
