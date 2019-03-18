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


buttonStyle isActive =
    [ centerX
    , padding 10
    , Background.color white
    , Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]
    , Border.width 1
    , Border.rounded 2
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ mouseOver
                    [ Background.color grey
                    ]
                ]
            else
                [ alpha 0.3
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                ]
           )


buttonStyle2 isActive =
    [ centerX
    , padding 10
    , Background.color white
    , Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]
    , Border.rounded 2
    , mouseOver
        [ Background.color darkYellow

        --, Border.shadow
        --    { offset = ( 0, 0 )
        --    , size = 1
        --    , blur = 0
        --    , color = rgb255 47 79 79
        --    }
        ]
    ]


iconsStyle =
    [ Border.width 1
    , Border.color white
    , Border.rounded 5
    , Background.color white
    , mouseOver
        [ Border.color black ]
    ]


sides =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


okMark =
    el
        [ Font.bold
        , Font.color (rgb255 92 184 92)
        ]
        (text "✓")


redStar =
    el
        [ Font.color red
        , Font.size 18
        ]
        (text "*")


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
    [ width (px 200)
    , paddingXY 5 5

    --, spacing 15
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]



-------------------------------------------------------------------------------


type alias ImageMeta =
    { url : String
    , caption : Maybe String
    , size : { height : Int, width : Int }
    }


sameHeightImgRow : String -> Maybe (Int -> List (Attribute msg)) -> List ImageMeta -> Element msg
sameHeightImgRow baseUrl mbattrs images =
    let
        images_ =
            List.map
                (\meta ->
                    { meta = meta
                    , newWidth = 0
                    , newHeight = 0
                    }
                )
                images

        imgSizes imgs =
            List.map (\i -> i.meta.size) imgs

        minHeight imgs =
            imgSizes imgs
                |> List.map .height
                |> List.sort
                |> List.head
                |> Maybe.withDefault 0

        imgsScaledToMinHeight =
            let
                mh =
                    minHeight images_

                scale { meta } =
                    { meta = meta
                    , newHeight = toFloat mh + 5
                    , newWidth =
                        toFloat mh
                            * toFloat meta.size.width
                            / toFloat meta.size.height
                    }
            in
            List.map scale images_

        totalImgWidth =
            List.foldr (\i n -> i.newWidth + n) 0 imgsScaledToMinHeight

        extraAttrs index =
            case mbattrs of
                Just f ->
                    f index

                _ ->
                    []
    in
    Keyed.row
        [ spacing 15 ]
        (List.indexedMap
            (\i im ->
                ( String.fromInt (i * List.length imgsScaledToMinHeight)
                , column
                    ([ width <| fillPortion (floor <| 10000 * im.newWidth / totalImgWidth)
                     ]
                        ++ extraAttrs i
                    )
                    [ html <|
                        Html.img
                            [ HtmlAttr.style "width" "100%"
                            , HtmlAttr.style "height" "auto"
                            , HtmlAttr.src (baseUrl ++ im.meta.url)
                            ]
                            []
                    , paragraph []
                        [ text (baseUrl ++ im.meta.url) ]

                    --{ src = im.meta.url
                    --, description =
                    --    im.meta.caption
                    --        |> Maybe.withDefault ""
                    --}
                    ]
                )
            )
            imgsScaledToMinHeight
        )



-------------------------------------------------------------------------------


tabView : plugin -> plugin -> (plugin -> msg) -> String -> Element msg
tabView currentTool tool handler tabname =
    el
        ([ Events.onClick (handler tool)
         , Border.widthEach
            { top = 2
            , bottom = 0
            , left = 2
            , right = 2
            }
         , Border.roundEach
            { topLeft = 8
            , topRight = 8
            , bottomLeft = 0
            , bottomRight = 0
            }
         , if currentTool == tool then
            Background.color (rgb 1 1 1)
           else
            Background.color (rgb 0.9 0.9 0.9)
         ]
            ++ (if currentTool == tool then
                    [ Border.color (rgb 0.8 0.8 0.8)
                    ]
                else
                    [ Border.color (rgb 0.9 0.9 0.9)
                    , pointer
                    , mouseOver
                        [ Background.color (rgb 0.95 0.95 0.95)
                        ]
                    ]
               )
        )
        (el
            ([]
                ++ (if currentTool == tool then
                        [ Border.roundEach
                            { topLeft = 8
                            , topRight = 8
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , paddingEach
                            { top = 3
                            , bottom = 7
                            , left = 12
                            , right = 12
                            }
                        , Border.color (rgb 1 1 1)
                        , moveDown 2
                        , Background.color (rgb 1 1 1)
                        ]
                    else
                        [ paddingEach
                            { top = 5
                            , bottom = 5
                            , left = 12
                            , right = 12
                            }
                        ]
                   )
            )
            (text tabname)
        )



-------------------------------------------------------------------------------


progressBar : Int -> Element msg
progressBar n =
    row
        [ width (px 200)
        , height (px 25)
        , Border.innerShadow
            { offset = ( 0, 1 )
            , size = 1
            , blur = 1
            , color = rgb255 127 127 127
            }
        , Background.color (rgb255 245 245 245)
        , Border.rounded 5
        , clip
        , inFront <|
            el
                [ width (px 200)
                , height (px 25)
                , Font.center
                ]
                (el
                    [ centerX
                    , centerY
                    ]
                    (String.fromInt n
                        |> String.padLeft 2 '0'
                        |> strCons "%"
                        |> text
                    )
                )
        ]
        [ el
            [ width (fillPortion n)
            , height fill
            , Background.color
                (if n < 25 then
                    rgb255 217 83 79
                 else if n < 50 then
                    rgb255 240 173 78
                 else if n < 75 then
                    rgb255 91 192 222
                 else
                    rgb255 92 184 92
                )
            , Font.center
            ]
            Element.none
        , el
            [ width (fillPortion (100 - n))
            , height fill
            ]
            Element.none
        ]


strCons : String -> String -> String
strCons tail head =
    head ++ tail
