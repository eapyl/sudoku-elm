module Update exposing (update)

import Model exposing (Model, emptyModel)
import Msg exposing (Msg(..))
import Sudoku.Update exposing (createBoard, setComplexity, update)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SudokuCommand subMsg ->
            Sudoku.Update.update subMsg model.sudoku
                |> updateWith (\subModel -> { model | sudoku = subModel }) SudokuCommand

        ChangeLevel newLevel ->
            let
                updatedSudoku =
                    setComplexity newLevel
            in
            ( { emptyModel | sudoku = updatedSudoku }
            , Cmd.map SudokuCommand createBoard
            )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
