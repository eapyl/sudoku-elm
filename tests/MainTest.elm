module MainTest exposing (getColumnTest, getRowTest)

import Array
import Expect
import Main exposing (Board, RowColumnIndex(..), Value(..), initZeroSudokuBoard)
import Test exposing (Test, describe, test)


getColumnTest : Test
getColumnTest =
    describe "Get Column"
        [ test "correct 2 column" <|
            \_ ->
                let
                    board =
                        initZeroSudokuBoard

                    secondColumn =
                        getColumn board Second

                    expectedColumn =
                        Array.fromList [ Two, Two, Two, Two, Two, Two, Two, Two, Two ]
                in
                Expect.equal expectedColumn secondColumn
        ]


getRowTest : Test
getRowTest =
    describe "Get Row"
        [ test "correct 2 row" <|
            \_ ->
                let
                    board =
                        getBoard

                    secondRow =
                        getRow board Second

                    expectedRow =
                        Array.fromList [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]
                in
                Expect.equal expectedRow secondRow
        ]


getBoard : Board
getBoard =
    Array.fromList
        [ One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        ]
