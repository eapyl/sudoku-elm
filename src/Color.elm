module Color exposing
    ( blue
    , gray100
    , gray200
    , gray300
    , gray400
    , gray500
    , green600
    , red600
    , white
    )

import Element exposing (Color, rgb255)
import Hex


hexToColor : String -> Color
hexToColor hexValue =
    let
        hex =
            String.toLower hexValue
    in
    if String.length hex == 6 then
        let
            f1 =
                String.slice 0 2 hex
                    |> Hex.fromString

            f2 =
                String.slice 2 4 hex |> Hex.fromString

            f3 =
                String.slice 4 6 hex |> Hex.fromString
        in
        case ( f1, f2, f3 ) of
            ( Ok f11, Ok f22, Ok f33 ) ->
                rgb255 f11 f22 f33

            _ ->
                white

    else
        white


blue : Color
blue =
    hexToColor "1293D8"


white : Color
white =
    rgb255 255 255 255


gray100 : Color
gray100 =
    hexToColor "f7fafc"


red600 : Color
red600 =
    hexToColor "E53E3E"


gray200 : Color
gray200 =
    hexToColor "edf2f7"


gray300 : Color
gray300 =
    hexToColor "e2e8f0"


gray400 : Color
gray400 =
    hexToColor "cbd5e0"


gray500 : Color
gray500 =
    hexToColor "a0aec0"


green600 : Color
green600 =
    hexToColor "38A169"
