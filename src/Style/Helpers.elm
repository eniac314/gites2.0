module Style.Helpers exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html as Html
import Html.Attributes as HtmlAttr
import Style.Palette exposing (..)


noAttr =
    htmlAttribute <| HtmlAttr.class ""


longText =
    "Il était une fois, au milieu d'une forêt épaisse, une petite maison où habitait une jolie petite fille nommée Petit Chaperon Rouge. Un jour ensoleillé, sa maman l'appela dans la cuisine de leur petite maison."


buttonStyle =
    [ centerX
    , padding 20
    , Font.family
        [ Font.external
            { name = "Montserrat"
            , url = "https://fonts.googleapis.com/css?family=Montserrat"
            }
        , Font.sansSerif
        ]
    , mouseOver
        [ Background.color lightYellow ]
    ]


iconsStyle =
    [ mouseOver
        [ Background.color lightYellow ]
    ]


sides =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


unselectable =
    List.map htmlAttribute
        [ HtmlAttr.style "-webkit-touch-callout" "none"
        , HtmlAttr.style "-webkit-user-select" "none"
        , HtmlAttr.style "-khtml-user-select" "none"
        , HtmlAttr.style "-moz-user-select" "none"
        , HtmlAttr.style "-ms-user-select" "none"
        , HtmlAttr.style "user-select" "none"
        ]
