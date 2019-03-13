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
import Html as Html
import Html.Attributes as HtmlAttr
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.ImageController as ImageController
import Internals.MarkdownEditor as MarkdownEditor
import Internals.MarkdownParser as MarkdownParser
import Json.Decode as D
import Json.Encode as E
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
    { content : Dict Int FrontPageItem
    , markdownEditor : MarkdownEditor.Model msg
    , imageController : ImageController.Model msg
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
    | ImageControllerMsg ImageController.Msg
    | GotFrontPageContent (Result Http.Error FrontPageContent)
    | SaveFrontPage
    | FrontPageSaved (Result Http.Error ())
    | NoOp


init : FrontPageContent -> (Msg -> msg) -> ( Model msg, Cmd msg )
init content outMsg =
    let
        ( imageController, imgCtrlCmd ) =
            ImageController.init
                "frontPage"
                (outMsg << ImageControllerMsg)
    in
    ( { content =
            List.indexedMap Tuple.pair content
                |> Dict.fromList
      , markdownEditor =
            MarkdownEditor.init
                French
                (outMsg << MarkdownEditorMsg)
      , imageController = imageController
      , displayMode = Preview
      , selectedItem = Nothing
      , outMsg = outMsg
      }
    , Cmd.batch
        [ imgCtrlCmd
        , getFrontPageContent outMsg
        ]
    )


subscriptions model =
    Sub.batch
        [ ImageController.subscriptions model.imageController ]


update : { a | logInfo : LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        EditItem id ->
            case Dict.get id model.content of
                Just item ->
                    case item of
                        MarkdownContent markDownMls ->
                            ( { model
                                | markdownEditor =
                                    MarkdownEditor.load
                                        model.markdownEditor
                                        (Just markDownMls)
                                , displayMode = EditMarkdown
                                , selectedItem = Just id
                              }
                            , Cmd.none
                            )

                        ImageRow pics ->
                            let
                                ( imgCtrl, imgCtrlCmd ) =
                                    ImageController.load
                                        config.logInfo
                                        model.imageController
                                        pics
                            in
                            ( { model
                                | imageController = imgCtrl
                                , displayMode = EditPicRow
                                , selectedItem = Just id
                              }
                            , imgCtrlCmd
                            )

                        NewsBlock ->
                            ( { model
                                | displayMode = EditNews
                                , selectedItem = Just id
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        DeleteItem id ->
            ( { model | content = remove id model.content }
            , Cmd.none
            )

        SwapUp id ->
            let
                newContent =
                    Dict.values model.content
                        |> swapAt id (id - 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            ( { model | content = newContent }
            , Cmd.none
            )

        SwapDown id ->
            let
                newContent =
                    Dict.values model.content
                        |> swapAt id (id + 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            ( { model | content = newContent }
            , Cmd.none
            )

        NewMarkdown ->
            ( { model
                | markdownEditor =
                    MarkdownEditor.load
                        model.markdownEditor
                        Nothing
                , displayMode = EditMarkdown
                , selectedItem = Just (nextId model.content)
              }
            , Cmd.none
            )

        NewPicRow ->
            let
                ( imgCtrl, imgCtrlCmd ) =
                    ImageController.load
                        config.logInfo
                        model.imageController
                        []
            in
            ( { model
                | imageController = imgCtrl
                , displayMode = EditPicRow
                , selectedItem = Just (nextId model.content)
              }
            , imgCtrlCmd
            )

        AddNewsBlock ->
            ( { model
                | displayMode = EditNews
                , selectedItem = Just (nextId model.content)
              }
            , Cmd.none
            )

        MarkdownEditorMsg markdownEditorMsg ->
            let
                ( newEditor, mbPluginRes ) =
                    MarkdownEditor.update markdownEditorMsg model.markdownEditor
            in
            case mbPluginRes of
                Nothing ->
                    ( { model | markdownEditor = newEditor }
                    , Cmd.none
                    )

                Just PluginQuit ->
                    ( { model
                        | markdownEditor = newEditor
                        , displayMode = Preview
                      }
                    , Cmd.none
                    )

                Just (PluginData data) ->
                    case model.selectedItem of
                        Nothing ->
                            ( { model
                                | markdownEditor = newEditor
                                , displayMode = Preview
                              }
                            , Cmd.none
                            )

                        Just id ->
                            ( { model
                                | markdownEditor = newEditor
                                , content =
                                    Dict.insert
                                        id
                                        (MarkdownContent data)
                                        model.content
                                , displayMode = Preview
                              }
                            , Cmd.none
                            )

        ImageControllerMsg imageControllerMsg ->
            let
                ( newImgCtrl, cmd, mbPluginRes ) =
                    ImageController.update
                        config
                        imageControllerMsg
                        model.imageController
            in
            case mbPluginRes of
                Nothing ->
                    ( { model | imageController = newImgCtrl }
                    , cmd
                    )

                Just PluginQuit ->
                    ( { model
                        | imageController = newImgCtrl
                        , displayMode = Preview
                      }
                    , cmd
                    )

                Just (PluginData data) ->
                    case model.selectedItem of
                        Nothing ->
                            ( { model
                                | imageController = newImgCtrl
                                , displayMode = Preview
                              }
                            , cmd
                            )

                        Just id ->
                            ( { model
                                | imageController = newImgCtrl
                                , content =
                                    Dict.insert
                                        id
                                        (ImageRow data)
                                        model.content
                                , displayMode = Preview
                              }
                            , cmd
                            )

        GotFrontPageContent res ->
            case res of
                Ok content ->
                    ( { model
                        | content =
                            List.indexedMap Tuple.pair content
                                |> Dict.fromList
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

        SaveFrontPage ->
            ( model, saveFrontPage config.logInfo model )

        FrontPageSaved res ->
            case res of
                Ok _ ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
            ImageController.view config model.imageController

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
            , padding 10
            , spacing 10
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            ]
            [ Input.button
                (buttonStyle True)
                { onPress = Just NewMarkdown
                , label =
                    textM config.lang
                        { en = "New article"
                        , fr = "Nouvel article"
                        }
                }
            , Input.button
                (buttonStyle True)
                { onPress = Just NewPicRow
                , label =
                    textM config.lang
                        { en = "Add images"
                        , fr = "Ajouter images"
                        }
                }
            , Input.button
                (buttonStyle True)
                { onPress = Just AddNewsBlock
                , label =
                    textM config.lang
                        { en = "Add news"
                        , fr = "Ajouter actualités"
                        }
                }
            , Input.button
                (buttonStyle True ++ [ alignRight ])
                { onPress = Just SaveFrontPage
                , label =
                    textM config.lang
                        { en = "Save"
                        , fr = "Sauvegarder"
                        }
                }
            ]
        , if model.content == Dict.empty then
            Element.none
          else
            Dict.map (editableItem config) model.content
                |> Dict.values
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
        , Border.color lightGray
        , Border.rounded 1
        , mouseOver
            [ Border.color lightCharcoal ]
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
                sameHeightImgRow awsUrl Nothing images

        NewsBlock ->
            Element.none


itemControlView : ViewConfig -> Int -> Element Msg
itemControlView config id =
    row
        [ spacing 15
        , padding 10
        , Border.width 1
        , Border.rounded 3
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
-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------


getFrontPageContent : (Msg -> msg) -> Cmd msg
getFrontPageContent outMsg =
    Http.get
        { url = "api/pagesdata/frontPage"
        , expect =
            Http.expectJson
                (outMsg << GotFrontPageContent)
                decodeFrontPage
        }


saveFrontPage : LogInfo -> Model msg -> Cmd msg
saveFrontPage logInfo model =
    let
        body =
            Dict.values model.content
                |> (\c ->
                        E.object
                            [ ( "name", E.string "frontPage" )
                            , ( "content", encodeFrontPage c )
                            ]
                            |> Http.jsonBody
                   )
    in
    securePost logInfo
        { url = "api/restricted/pagesdata"
        , body = body
        , expect =
            Http.expectWhatever (model.outMsg << FrontPageSaved)
        }


encodeFrontPage : FrontPageContent -> E.Value
encodeFrontPage fpi =
    E.list encodeFrontPageItem fpi
        |> E.encode 0
        |> E.string


decodeFrontPage : D.Decoder FrontPageContent
decodeFrontPage =
    D.field "data" <|
        D.field "content"
            (D.string
                |> D.map (D.decodeString (D.list decodeFrontPageItem))
                |> D.map (Result.withDefault [])
            )


encodeFrontPageItem : FrontPageItem -> E.Value
encodeFrontPageItem fpi =
    case fpi of
        MarkdownContent mls ->
            E.object
                [ ( "MarkdownContent", encodeMls mls ) ]

        ImageRow imgs ->
            E.object
                [ ( "ImageRow", E.list encodeImageMeta imgs ) ]

        NewsBlock ->
            E.string "NewsBlock"


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


encodeMls : MultLangStr -> E.Value
encodeMls { en, fr } =
    E.object
        [ ( "MultLangStr"
          , E.object
                [ ( "en", E.string en )
                , ( "fr", E.string fr )
                ]
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


encodeImageMeta : ImageMeta -> E.Value
encodeImageMeta { url, caption, size } =
    E.object
        [ ( "url", E.string url )
        , ( "caption"
          , Maybe.map E.string caption
                |> Maybe.withDefault E.null
          )
        , ( "size", encodeSize size )
        ]


decodeImageMeta : D.Decoder ImageMeta
decodeImageMeta =
    D.map3 ImageMeta
        (D.field "url" D.string)
        (D.field "caption" (D.nullable D.string))
        (D.field "size" decodeSize)


encodeSize : { width : Int, height : Int } -> E.Value
encodeSize size =
    E.object
        [ ( "width", E.int size.width )
        , ( "height", E.int size.height )
        ]


decodeSize : D.Decoder { width : Int, height : Int }
decodeSize =
    D.map2 (\w h -> { width = w, height = h })
        (D.field "width" D.int)
        (D.field "height" D.int)
