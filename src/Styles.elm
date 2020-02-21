module Styles exposing
    ( btnCss
    , btnRightCss
    , divBottomCss
    , divInBtnCss
    , divInDivCss
    , divInTdCss
    , mainDivCss
    , tableCss
    , tdCss
    )

import Css
    exposing
        ( FontSize
        , Px
        , Style
        , absolute
        , after
        , auto
        , backgroundColor
        , block
        , border3
        , bottom
        , display
        , float
        , fontSize
        , height
        , hex
        , large
        , left
        , margin2
        , marginTop
        , maxWidth
        , pct
        , position
        , property
        , px
        , relative
        , rgb
        , right
        , solid
        , top
        , width
        , xLarge
        , xxLarge
        )
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


maxWidthValue : Px
maxWidthValue =
    px 500


tableStyle : Style
tableStyle =
    Css.batch
        [ width (pct 100)
        , maxWidth maxWidthValue
        ]


borderStyle : Style
borderStyle =
    border3 (px 1) solid (rgb 0 0 0)


centeredDiv : Float -> Float -> FontSize a -> Style
centeredDiv leftValue topValue size =
    Css.batch
        [ position relative
        , top (pct topValue)
        , left (pct leftValue)
        , fontSize size
        ]


grayBox : Style
grayBox =
    backgroundColor (hex "BEBEBE")


whiteBox : Style
whiteBox =
    backgroundColor (hex "FFFFFF")


mainDivCss : Attribute msg
mainDivCss =
    css
        [ margin2 (px 0) auto
        , maxWidth maxWidthValue
        ]


tdCss : Bool -> Attribute msg
tdCss gray =
    css
        [ borderStyle
        , width (pct 10)
        , position relative
        , after
            [ property "content" "\"\""
            , display block
            , marginTop (pct 100)
            ]
        , if gray then
            grayBox

          else
            whiteBox
        ]


tableCss : Attribute msg
tableCss =
    css [ tableStyle, borderStyle ]


divInTdCss : Attribute msg
divInTdCss =
    css
        [ position absolute
        , top (pct 50)
        , bottom (px 0)
        , left (pct 50)
        , right (px 0)
        ]


divInDivCss : Attribute msg
divInDivCss =
    css [ centeredDiv -35 -70 xxLarge ]


divInBtnCss : Attribute msg
divInBtnCss =
    css [ centeredDiv -50 -70 xLarge ]


divBottomCss : Attribute msg
divBottomCss =
    css [ marginTop (px 10) ]


btnStyle : List Style
btnStyle =
    [ height (px 50)
    , width (px 80)
    , fontSize large
    ]


btnCss : Attribute msg
btnCss =
    css
        btnStyle


btnRightCss : Attribute msg
btnRightCss =
    css
        (float right :: btnStyle)
