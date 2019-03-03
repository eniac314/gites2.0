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


buttonStyleSha =
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
        [ Border.shadow
            { offset = ( 0, 0 )
            , size = 2
            , blur = 0
            , color = (rgb255 47 79 79)
            }
        ]
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



-------------------------------------------------------------------------------


buttonStyle_ isActive =
    [ Border.rounded 5
    , Font.center
    , centerY
    , padding 5
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ Background.color (rgb 0.9 0.9 0.9)
                , mouseOver [ Font.color (rgb 255 255 255) ]
                , Border.width 1
                , Border.color (rgb 0.9 0.9 0.9)
                ]
            else
                [ Background.color (rgb 0.95 0.95 0.95)
                , Font.color (rgb 0.7 0.7 0.7)
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                , Border.width 1
                , Border.color (rgb 0.95 0.95 0.95)
                ]
           )


toogleButtonStyle_ isPressed isActive =
    [ Border.rounded 5
    , Font.center
    , centerY
    , padding 5
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ Background.color (rgb 0.9 0.9 0.9)
                , Border.width 1
                , Border.color (rgb 0.9 0.9 0.9)
                , mouseOver
                    [ Font.color (rgb 0.3 0.3 0.3)
                    ]
                ]
                    ++ (if isPressed then
                            []
                        else
                            [ Background.color (rgb 1 1 1)
                            , Border.width 1
                            , Border.color (rgb 0.9 0.9 0.9)
                            ]
                       )
            else
                [ Background.color (rgb 0.95 0.95 0.95)
                , Font.color (rgb 0.7 0.7 0.7)
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                , Border.width 1
                , Border.color (rgb 0.95 0.95 0.95)
                ]
           )


textInputStyle_ =
    [ width (px 250)
    , paddingXY 5 5
    , spacing 15
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
