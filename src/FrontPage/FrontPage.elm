module FrontPage.FrontPage exposing (..)

import Browser exposing (element)
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
import Html as Html
import Html.Attributes as HtmlAttr
import Internals.MarkdownParser exposing (..)
import List.Extra exposing (swapAt)
import MultLang.MultLang exposing (..)
import Style.Palette exposing (..)


main : Program () (Model Msg) Msg
main =
    Browser.element
        { init = \_ -> ( init [] identity, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = \model -> Element.layout [] <| view { lang = English } model
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch []


type alias FrontPageContent =
    List FrontPageItem


type FrontPageItem
    = MarkdownContent MultLangStr
    | ImageRow (List ImageMeta)
    | NewsBlock


type alias ImageMeta =
    { url : String
    , caption : Maybe String
    , size : { height : Int, width : Int }
    }


type alias Model msg =
    { markDownInputs : Maybe MultLangStr
    , picRowInput : List ImageMeta
    , content : Dict Int FrontPageItem
    , displayMode : DisplayMode
    , selectedItem : Maybe Int
    , outMsg : Msg -> msg
    }


type DisplayMode
    = Preview
    | EditMarkdown
    | EditPicRow
    | EditNews


type Msg
    = EditItem Int
    | DeleteItem Int
    | SwapUp Int
    | SwapDown Int
    | NewMarkdown
    | NewPicRow
    | AddNewsBlock
    | NoOp


init : FrontPageContent -> (Msg -> msg) -> Model msg
init content outMsg =
    { markDownInputs = Nothing
    , picRowInput = []
    , content =
        List.indexedMap Tuple.pair content
            |> Dict.fromList
    , displayMode = Preview
    , selectedItem = Nothing
    , outMsg = outMsg
    }


update : Msg -> Model msg -> Model msg
update msg model =
    case msg of
        EditItem id ->
            case Dict.get id model.content of
                Just item ->
                    case item of
                        MarkdownContent markDownMls ->
                            { model
                                | markDownInputs = Just markDownMls
                                , displayMode = EditMarkdown
                                , selectedItem = Just id
                            }

                        ImageRow pics ->
                            { model
                                | picRowInput = pics
                                , displayMode = EditPicRow
                                , selectedItem = Just id
                            }

                        NewsBlock ->
                            { model
                                | displayMode = EditNews
                                , selectedItem = Just id
                            }

                Nothing ->
                    model

        DeleteItem id ->
            { model | content = remove id model.content }

        SwapUp id ->
            let
                newContent =
                    Dict.values model.content
                        |> swapAt id (id - 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            { model | content = newContent }

        SwapDown id ->
            let
                newContent =
                    Dict.values model.content
                        |> swapAt id (id + 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            { model | content = newContent }

        NewMarkdown ->
            { model
                | markDownInputs = Nothing
                , displayMode = EditMarkdown
                , selectedItem = Just (nextId model.content)
            }

        NewPicRow ->
            { model
                | picRowInput = []
                , displayMode = EditPicRow
                , selectedItem = Just (nextId model.content)
            }

        AddNewsBlock ->
            { model
                | displayMode = EditNews
                , selectedItem = Just (nextId model.content)
            }

        NoOp ->
            model


nextId : Dict Int a -> Int
nextId dict =
    1
        + Dict.foldr
            (\k v acc ->
                max k acc
            )
            0
            dict


type alias ViewConfig =
    { lang : Lang
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        case model.displayMode of
            Preview ->
                previewView config model

            EditMarkdown ->
                Element.none

            EditPicRow ->
                Element.none

            EditNews ->
                Element.none


previewView : ViewConfig -> Model msg -> Element Msg
previewView config model =
    column
        [ width fill ]
        [ row
            [ width fill ]
            [ Input.button
                []
                { onPress = Just NewMarkdown
                , label =
                    textM config.lang
                        { en = "New article"
                        , fr = "Nouvel article"
                        }
                }
            , Input.button
                []
                { onPress = Just NewPicRow
                , label =
                    textM config.lang
                        { en = "Add images"
                        , fr = "Ajouter images"
                        }
                }
            , Input.button
                []
                { onPress = Just AddNewsBlock
                , label =
                    textM config.lang
                        { en = "Add news"
                        , fr = "Ajouter actualités"
                        }
                }
            ]
        ]
