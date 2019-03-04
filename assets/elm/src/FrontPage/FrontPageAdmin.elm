module FrontPage.FrontPageAdmin exposing (..)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost)
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
import FrontPage.MarkdownEditor as MarkdownEditor exposing (..)
import Html as Html
import Html.Attributes as HtmlAttr
import Internals.Helpers exposing (..)
import Internals.MarkdownParser as MarkdownParser exposing (..)
import List.Extra exposing (swapAt)
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
import Style.Palette exposing (..)


type alias FrontPageContent =
    List FrontPageItem


type FrontPageItem
    = MarkdownContent MultLangStr
    | ImageRow (List ImageMeta)
    | NewsBlock


type alias Model msg =
    { picRowInput : List ImageMeta
    , content : Dict Int FrontPageItem
    , markdownEditor : MarkdownEditor.Model msg
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
    | MarkdownEditorMsg MarkdownEditor.Msg
    | NoOp


init : FrontPageContent -> (Msg -> msg) -> Model msg
init content outMsg =
    { picRowInput = []
    , content =
        List.indexedMap Tuple.pair content
            |> Dict.fromList
    , markdownEditor =
        MarkdownEditor.init
            French
            (outMsg << MarkdownEditorMsg)
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
                                | markdownEditor =
                                    MarkdownEditor.load
                                        model.markdownEditor
                                        (Just markDownMls)
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
                | markdownEditor =
                    MarkdownEditor.load
                        model.markdownEditor
                        Nothing
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

        MarkdownEditorMsg markdownEditorMsg ->
            let
                ( newEditor, mbPluginRes ) =
                    MarkdownEditor.update markdownEditorMsg model.markdownEditor
            in
            case mbPluginRes of
                Nothing ->
                    { model | markdownEditor = newEditor }

                Just PluginQuit ->
                    { model
                        | markdownEditor = newEditor
                        , displayMode = Preview
                    }

                Just (PluginData data) ->
                    case model.selectedItem of
                        Nothing ->
                            { model
                                | markdownEditor = newEditor
                                , displayMode = Preview
                            }

                        Just id ->
                            { model
                                | markdownEditor = newEditor
                                , content =
                                    Dict.insert
                                        id
                                        (MarkdownContent data)
                                        model.content
                                , displayMode = Preview
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
    , width : Int
    , logInfo : LogInfo
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    case model.displayMode of
        Preview ->
            Element.map model.outMsg <|
                previewView config model

        EditMarkdown ->
            MarkdownEditor.view config model.markdownEditor

        EditPicRow ->
            Element.none

        EditNews ->
            Element.none


previewView : ViewConfig -> Model msg -> Element Msg
previewView config model =
    column
        [ width fill
        , height fill
        , paddingEach { top = 0, right = 45, bottom = 45, left = 45 }
        , spacing 30
        , Background.color lightGrey
        ]
        [ row
            [ centerX
            , width fill
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            ]
            [ Input.button
                buttonStyleSha
                { onPress = Just NewMarkdown
                , label =
                    textM config.lang
                        { en = "New article"
                        , fr = "Nouvel article"
                        }
                }
            , Input.button
                buttonStyle
                { onPress = Just NewPicRow
                , label =
                    textM config.lang
                        { en = "Add images"
                        , fr = "Ajouter images"
                        }
                }
            , Input.button
                buttonStyle
                { onPress = Just AddNewsBlock
                , label =
                    textM config.lang
                        { en = "Add news"
                        , fr = "Ajouter actualités"
                        }
                }
            ]
        , if model.content == Dict.empty then
            Element.none
          else
            Dict.map (editableItem config) model.content
                --Dict Int (Element Msg)
                |> Dict.values
                --List (Element Msg)
                |> column
                    [ Background.color grey
                    , width fill
                    , centerX
                    , padding 10
                    , spacing 20
                    ]
        ]


editableItem : ViewConfig -> Int -> FrontPageItem -> Element Msg
editableItem config id item =
    row
        [ centerX
        , spacing 30
        , padding 20
        , Border.width 1
        , Border.color lightCharcoal
        , Border.rounded 1
        , mouseOver
            [ Border.color lightGray ]
        ]
        [ frontPageItemView config item
        , itemControlView config id
        ]



--frontPageContentView : config model


frontPageItemView : ViewConfig -> FrontPageItem -> Element msg
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
                                []
                                { src = url
                                , description =
                                    caption
                                        |> Maybe.withDefault ""
                                }
                        )
                        images
                    )
            else
                sameHeightImgRow config.width images

        NewsBlock ->
            Element.none


itemControlView : ViewConfig -> Int -> Element Msg
itemControlView config id =
    row
        [ spacing 15
        , padding 5
        , Border.width 1
        , Border.rounded 1
        ]
        [ column
            [ spacing 10 ]
            [ Input.button
                iconsStyle
                { onPress = Just <| SwapUp id
                , label =
                    Icons.arrowUp
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                }
            , Input.button
                iconsStyle
                { onPress = Just <| SwapDown id
                , label =
                    Icons.arrowDown
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                }
            ]
        , column
            [ spacing 10 ]
            [ Input.button
                iconsStyle
                { onPress = Just <| EditItem id
                , label =
                    Icons.pencil
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                }
            , Input.button
                iconsStyle
                { onPress = Just <| DeleteItem id
                , label =
                    Icons.x
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                }
            ]
        ]



--newsBlockView config
