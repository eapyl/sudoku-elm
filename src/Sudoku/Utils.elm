module Sudoku.Utils exposing (allIndexes, allValues, fromInt, indexToInt, toString)

import Sudoku.Model exposing (Index(..), Value(..))


allValues : List Value
allValues =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]


allIndexes : List Index
allIndexes =
    [ First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth, Ninth ]


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


toString : Value -> String
toString value =
    case value of
        Empty ->
            " "

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


indexToInt : Index -> Int
indexToInt index =
    case index of
        First ->
            0

        Second ->
            1

        Third ->
            2

        Fourth ->
            3

        Fifth ->
            4

        Sixth ->
            5

        Seventh ->
            6

        Eighth ->
            7

        Ninth ->
            8
