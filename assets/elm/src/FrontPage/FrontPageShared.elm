module FrontPage.FrontPageShared exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Http exposing (..)
import Internals.Helpers exposing (awsUrl)
import Internals.MarkdownParser as MarkdownParser
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)


type alias FrontPageContent =
    List FrontPageItem


type FrontPageItem
    = MarkdownContent MultLangStr
    | ImageRow (List ImageMeta)
    | NewsBlock


frontPageItemView : { a | lang : Lang, width : Int } -> FrontPageItem -> Element msg
frontPageItemView config item =
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
                                    caption
                                        |> Maybe.withDefault ""
                                }
                        )
                        images
                    )
            else
                sameHeightImgRow awsUrl Nothing images

        NewsBlock ->
            Element.none


getFrontPageContent : (Result Error FrontPageContent -> msg) -> Cmd msg
getFrontPageContent responseHandler =
    Http.get
        { url = "api/pagesdata/frontPage"
        , expect =
            Http.expectJson
                responseHandler
                decodeFrontPage
        }


decodeFrontPage : D.Decoder FrontPageContent
decodeFrontPage =
    D.field "data" <|
        D.field "content"
            (D.list decodeFrontPageItem)


decodeFrontPageItem : D.Decoder FrontPageItem
decodeFrontPageItem =
    D.oneOf
        [ D.field "MarkdownContent" decodeMls
            |> D.map MarkdownContent
        , D.field "ImageRow" (D.list decodeImageMeta)
            |> D.map ImageRow
        , D.string
            |> D.andThen
                (\str ->
                    case str of
                        "NewsBlock" ->
                            D.succeed NewsBlock

                        somethingElse ->
                            D.fail <|
                                "Unknown CellContent: "
                                    ++ somethingElse
                )
        ]


decodeMls : D.Decoder MultLangStr
decodeMls =
    D.field
        "MultLangStr"
    <|
        D.map2 MultLangStr
            (D.field "en" D.string)
            (D.field "fr" D.string)


decodeImageMeta : D.Decoder ImageMeta
decodeImageMeta =
    D.map3 ImageMeta
        (D.field "url" D.string)
        (D.field "caption" (D.nullable D.string))
        (D.field "size" decodeSize)


decodeSize : D.Decoder { width : Int, height : Int }
decodeSize =
    D.map2 (\w h -> { width = w, height = h })
        (D.field "width" D.int)
        (D.field "height" D.int)
