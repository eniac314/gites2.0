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
import Internals.Helpers exposing (awsUrl)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
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


getGalleryMetas : (Result Error (Dict String GalleryMeta) -> msg) -> Cmd msg
getGalleryMetas responseHandler =
    Http.get
        { url = "api/pagesdata/galleryMetas"
        , expect =
            Http.expectJson
                responseHandler
                decodeGalleryMetas
        }


decodeGalleryMetas : Decode.Decoder (Dict String GalleryMeta)
decodeGalleryMetas =
    Decode.list decodeGalleryMeta
        |> Decode.map (List.map (\g -> ( g.title.en, g )))
        |> Decode.map Dict.fromList


decodeGalleryMeta : Decode.Decoder GalleryMeta
decodeGalleryMeta =
    Decode.succeed GalleryMeta
        |> required "title" decodeMls
        |> required "titleImg" (Decode.nullable Decode.string)
        |> required "article" (Decode.nullable decodeMls)
        |> required "album" (Decode.list decodeImageMeta)


imgBlockView handler ( title, titleImg ) =
    column
        [ spacing 10
        , padding 10
        , width (px 200)
        , alignTop
        , mouseOver
            [ alpha 0.7 ]
        , Border.rounded 5
        , Events.onClick (handler title)
        ]
        [ el
            [ width (px 190)
            , height (px 190)
            , centerX
            , Background.image <|
                awsUrl
                    ++ title
                    ++ "/thumbs/"
                    ++ titleImg
            ]
            Element.none
        ]
