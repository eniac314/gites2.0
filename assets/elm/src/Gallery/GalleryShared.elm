module Gallery.GalleryShared exposing (..)

import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Element.Region as Region
import FrontPage.FrontPageShared exposing (decodeImageMeta, decodeMls)
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Http exposing (..)
import Internals.Helpers exposing (awsUrl, thumbSrc)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)


type Direction
    = AnimateLeft
    | AnimateRight


type Drag
    = Drag Position Position


type alias Position =
    { x : Int
    , y : Int
    }


events : Maybe Drag -> ( Position -> msg, Position -> msg, msg ) -> List (Attribute msg)
events drag ( dragStart, dragAt, dragEnd ) =
    List.map htmlAttribute <|
        moveEvent drag ( dragStart, dragAt, dragEnd )
            ++ [ HtmlEvents.on "mousedown" (Decode.map dragStart decodePosition)
               , HtmlEvents.on "touchstart" (Decode.map dragStart decodePosition)
               ]


moveEvent : Maybe a -> ( Position -> msg, Position -> msg, msg ) -> List (Html.Attribute msg)
moveEvent drag ( dragStart, dragAt, dragEnd ) =
    case drag of
        Just _ ->
            [ HtmlEvents.preventDefaultOn "mousemove"
                (Decode.map (\p -> ( dragAt p, True )) decodePosition)
            , HtmlEvents.preventDefaultOn "touchmove"
                (Decode.map (\p -> ( dragAt p, True )) decodePosition)
            , HtmlEvents.on "mouseup" (Decode.succeed dragEnd)
            , HtmlEvents.on "mouseleave" (Decode.succeed dragEnd)
            , HtmlEvents.on "touchend" (Decode.succeed dragEnd)
            , HtmlEvents.on "touchcancel" (Decode.succeed dragEnd)
            ]

        Nothing ->
            []


decodePosition : Decode.Decoder Position
decodePosition =
    let
        decoder =
            Decode.map2 Position
                (Decode.field "pageX" (Decode.map floor Decode.float))
                (Decode.field "pageY" (Decode.map floor Decode.float))
    in
    Decode.oneOf [ decoder, Decode.at [ "touches", "0" ] decoder ]



-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------


getDetailsPage : (Result Error DetailsPage -> msg) -> Cmd msg
getDetailsPage responseHandler =
    Http.get
        { url = "/api/pagesdata/details"
        , expect =
            Http.expectJson
                responseHandler
                decodePage
        }


type alias DetailsPage =
    { mainArticle : Maybe MultLangStr
    , galleries : Dict String GalleryMeta
    }


decodePage =
    Decode.field "data" <|
        Decode.field "content"
            (Decode.map2
                DetailsPage
                (Decode.field "mainArticle" (Decode.nullable decodeMls))
                (Decode.field "galleries" decodeGalleryMetas)
            )


decodeGalleryMetas : Decode.Decoder (Dict String GalleryMeta)
decodeGalleryMetas =
    Decode.list decodeGalleryMeta
        |> Decode.map (List.map (\g -> ( g.key, g )))
        |> Decode.map Dict.fromList


decodeGalleryMeta : Decode.Decoder GalleryMeta
decodeGalleryMeta =
    Decode.succeed GalleryMeta
        |> required "title" decodeMls
        |> required "key" Decode.string
        |> required "ordering" Decode.int
        |> required "titleImg" (Decode.nullable Decode.string)
        |> required "header" (Decode.nullable decodeMls)
        |> required "article" (Decode.nullable decodeMls)
        |> required "album" (Decode.list decodeImageMeta)


imgBlockView lang handler gm =
    column
        [ width (px 200)
        , height (px 200)
        , mouseOver
            [ alpha 0.7 ]
        , Events.onClick (handler gm.key)
        , Background.image <|
            awsUrl
                ++ thumbSrc (Maybe.withDefault "" gm.titleImg)
        , pointer
        ]
        [ el
            [ width (px 187)
            , height (px 187)
            , centerX
            , centerY
            , Background.color (rgba 1 1 1 0.6)
            , Border.width 3
            , Border.color (rgb255 101 51 61)
            ]
            (paragraph
                [ centerX
                , centerY
                , Font.color (rgb255 101 51 61)
                , Font.size 24
                , Font.center
                , Font.family
                    [ Font.typeface "times"
                    ]
                ]
                [ strM lang gm.title
                    |> String.toUpper
                    |> text
                ]
            )
        ]
