module GenericPage.GenericPageShared exposing (..)

import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Gallery.Carousel as Carousel
import Html as Html
import Html.Attributes as HtmlAttr
import Http exposing (..)
import Internals.Helpers exposing (awsUrl, decodeImageMeta, decodeMls)
import Internals.MarkdownParser as MarkdownParser
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)


type alias GenericPageContent =
    List GenericPageItem


type GenericPageItem
    = MarkdownContent MultLangStr
    | ImageRow (List ImageMeta)
    | GoogleMap GoogleMapMeta
    | Carousel String (List ImageMeta)


type alias GoogleMapMeta =
    { frameBorder : Bool
    , pb : String
    , height : Int
    , width : Int
    , url : String
    }


genericPageItemView :
    { a
        | lang : Lang
        , width : Int
        , carousels : Dict String (Carousel.Model msg)
        , downloadHandler : String -> msg
    }
    -> GenericPageItem
    -> Element msg
genericPageItemView config item =
    case item of
        MarkdownContent mls ->
            MarkdownParser.renderMarkdown
                (strM config.lang mls)
                config.downloadHandler

        ImageRow images ->
            if config.width < 1000 then
                column
                    [ spacing 15 ]
                    (List.map
                        (\{ url, caption } ->
                            image
                                [ width (maximum config.width fill) ]
                                { src = awsUrl ++ url
                                , description =
                                    Maybe.map (strM config.lang) caption
                                        |> Maybe.withDefault ""
                                }
                        )
                        images
                    )
            else
                sameHeightImgRow awsUrl Nothing images

        GoogleMap gm ->
            googleMapIframe gm

        Carousel id pics ->
            case Dict.get id config.carousels of
                Just carousel ->
                    Carousel.view { maxWidth = config.width } carousel

                Nothing ->
                    Element.none


getGenericPageContent : (Result Error GenericPageContent -> msg) -> String -> Cmd msg
getGenericPageContent responseHandler wd =
    Http.get
        { url = "/api/pagesdata/" ++ wd
        , expect =
            Http.expectJson
                responseHandler
                decodeGenericPage
        }


decodeGenericPage : D.Decoder GenericPageContent
decodeGenericPage =
    D.field "data" <|
        D.field "content"
            (D.list decodeGenericPageItem)


initCarousels : GenericPageContent -> (String -> Carousel.Msg -> msg) -> Dict String (Carousel.Model msg)
initCarousels pc outMsg =
    List.foldr
        (\pi acc ->
            case pi of
                Carousel id imgs ->
                    Dict.insert id
                        (Carousel.init (List.map (\i -> awsUrl ++ i.url) imgs)
                            (outMsg id)
                        )
                        acc

                _ ->
                    acc
        )
        Dict.empty
        pc


decodeGenericPageItem : D.Decoder GenericPageItem
decodeGenericPageItem =
    D.oneOf
        [ D.field "MarkdownContent" decodeMls
            |> D.map MarkdownContent
        , D.field "ImageRow" (D.list decodeImageMeta)
            |> D.map ImageRow
        , D.field "GoogleMap" decodeGoogleMapMeta
            |> D.map GoogleMap
        , D.field "Carousel"
            (D.map2 Carousel
                (D.field "id" D.string)
                (D.field "imgs" (D.list decodeImageMeta))
            )
        ]


decodeGoogleMapMeta : D.Decoder GoogleMapMeta
decodeGoogleMapMeta =
    D.map5 GoogleMapMeta
        (D.field "frameborder" D.bool)
        (D.field "pb" D.string)
        (D.field "height" D.int)
        (D.field "width" D.int)
        (D.field "url" D.string)



-------------------------------------------------------------------------------
----------------------
-- Google map stuff --
----------------------


parseHtml : String -> Maybe GoogleMapMeta
parseHtml str =
    let
        propDict =
            String.replace "<iframe " "" str
                |> String.replace "></iframe>" ""
                --|> String.dropRight 10
                |> String.words
                |> List.concatMap (String.split "?")
                |> List.map (String.split "=")
                |> List.filterMap
                    (\mbPair ->
                        case mbPair of
                            property :: [] ->
                                Just ( property, "True" )

                            property :: value :: [] ->
                                Just ( property, String.replace "\"" "" value )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
    in
    Just <|
        { frameBorder =
            Dict.get "frameborder" propDict
                |> Maybe.map (\v -> v == "1")
                |> Maybe.withDefault False
        , pb =
            Dict.get "pb" propDict
                |> Maybe.withDefault ""
        , width =
            Dict.get "width" propDict
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 560
        , height =
            Dict.get "height" propDict
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 315
        , url = str
        }


buildGoogleMapUrl mapMeta =
    let
        params =
            [ Just <| "pb=" ++ mapMeta.pb
            , if not mapMeta.frameBorder then
                Just "frameborder=0"
              else
                Nothing
            ]
                |> List.filterMap identity
                |> String.join "&"
                |> (\s ->
                        if s == "" then
                            s
                        else
                            "?" ++ s
                   )
    in
    "https://www.google.com/maps/embed"
        ++ params


googleMapIframe gm =
    el [ centerX ]
        (html <|
            Html.iframe
                [ HtmlAttr.src <|
                    buildGoogleMapUrl gm
                , HtmlAttr.width gm.width
                , HtmlAttr.height gm.height
                , if gm.frameBorder then
                    noHtmlAttr
                  else
                    HtmlAttr.attribute "frameborder" "0"
                , HtmlAttr.attribute "allowfullscreen" "true"
                ]
                []
        )
