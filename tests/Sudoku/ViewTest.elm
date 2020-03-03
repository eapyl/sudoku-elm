module Sudoku.ViewTest exposing (viewTest)

import Element exposing (layout)
import Expect
import Sudoku.Update exposing (emptyModel)
import Sudoku.View exposing (view)
import Test exposing (Test, describe, test, todo)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)


viewTest : Test
viewTest =
    describe "View tests"
        [ describe "Status tests"
            [ test "Show status text if there is a status" <|
                \_ ->
                    view { emptyModel | status = Just "Status" }
                        |> layout []
                        |> Query.fromHtml
                        |> Query.findAll [ text "Status" ]
                        |> Query.count (Expect.equal 1)
            , test "Show default status text" <|
                \_ ->
                    view emptyModel
                        |> layout []
                        |> Query.fromHtml
                        |> Query.findAll [ text "Generating board" ]
                        |> Query.count (Expect.equal 1)
            ]
        , describe "Board test"
            [ describe "Board without modal window"
                [ todo "Default board is showed"
                , todo "Random board is showed"
                , todo "Show modal window command is called for free cells"
                , todo "No command is called for filled cells"
                ]
            , describe "Board with modal window"
                [ todo "Check that menu is displayed for each cell"
                ]
            , describe "Modal window actions"
                [ todo "Selected number command is called"
                , todo "Remove number command is called"
                , todo "Back command is called"
                , todo "No actions if clicked on empty cell (between empty and back buttons)"
                ]
            , describe "Board with selected numbers"
                [ todo "Impossible selected value is highlighter as red"
                , todo "Possible selected value is highlighter as green"
                ]
            ]
        ]
