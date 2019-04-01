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
    | GoogleMap String
    | Carousel String (List ImageMeta)


genericPageItemView :
    { a
        | lang : Lang
        , width : Int
        , carousels : Dict String (Carousel.Model msg)
    }
    -> GenericPageItem
    -> Element msg
genericPageItemView config item =
    case item of
        MarkdownContent mls ->
            MarkdownParser.renderMarkdown
                (strM config.lang mls)

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

        GoogleMap s ->
            Element.none

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
        , D.field "GoogleMap" D.string
            |> D.map GoogleMap
        , D.field "Carousel"
            (D.map2 Carousel
                (D.field "id" D.string)
                (D.field "imgs" (D.list decodeImageMeta))
            )
        ]
