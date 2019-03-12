module Style.Palette exposing (..)

import Color as Color
import Element as Element


colorConv : Color.Color -> Element.Color
colorConv c =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue c_.alpha
           )


red =
    colorConv Color.red


orange =
    colorConv Color.orange


yellow =
    colorConv Color.yellow


green =
    colorConv Color.green


blue =
    colorConv Color.blue


purple =
    colorConv Color.purple


brown =
    colorConv Color.brown


lightRed =
    colorConv Color.lightRed


lightOrange =
    colorConv Color.lightOrange


lightYellow =
    colorConv Color.lightYellow


lightGreen =
    colorConv Color.lightGreen


lightBlue =
    colorConv Color.lightBlue


lightPurple =
    colorConv Color.lightPurple


lightBrown =
    colorConv Color.lightBrown


darkRed =
    colorConv Color.darkRed


darkOrange =
    colorConv Color.darkOrange


darkYellow =
    colorConv Color.darkYellow


darkGreen =
    colorConv Color.darkGreen


darkBlue =
    colorConv Color.darkBlue


darkPurple =
    colorConv Color.darkPurple


darkBrown =
    colorConv Color.darkBrown


white =
    colorConv Color.white


lightGrey =
    colorConv Color.lightGrey


grey =
    colorConv Color.grey


darkGrey =
    colorConv Color.darkGrey


lightCharcoal =
    colorConv Color.lightCharcoal


charcoal =
    colorConv Color.charcoal


darkCharcoal =
    colorConv Color.darkCharcoal


black =
    colorConv Color.black


lightGray =
    colorConv Color.lightGray


gray =
    colorConv Color.gray


darkGray =
    colorConv Color.darkGray



---------calendar color------


calRed =
    Element.rgb255 207 124 123


calGreen =
    Element.rgb255 163 192 74


calOrange =
    Element.rgb255 226 184 106
