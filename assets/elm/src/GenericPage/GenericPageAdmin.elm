module GenericPage.GenericPageAdmin exposing (DisplayMode(..), Model, Msg(..), ViewConfig, editableItem, encodeGenericPage, encodeGenericPageItem, encodeGoogleMapMeta, googleMapEditorView, init, itemControlView, previewView, saveGenericPage, subscriptions, update, view)

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
import File.Download as Download
import Gallery.Carousel as Carousel
import GenericPage.GenericPageShared exposing (..)
import Html as Html
import Html.Attributes as HtmlAttr
import Http exposing (..)
import Internals.DropdownSelect as Select exposing (..)
import Internals.Helpers exposing (..)
import Internals.ImageController as ImageController
import Internals.MarkdownEditor as MarkdownEditor
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (swapAt)
import MultLang.MultLang exposing (..)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
import Style.Palette exposing (..)


type alias Model msg =
    { workingDirectory : String
    , content : Dict Int GenericPageItem
    , markdownEditor : MarkdownEditor.Model msg
    , imageController : ImageController.Model msg
    , displayMode : DisplayMode
    , selectedItem : Maybe Int
    , currentSeed : Seed
    , currentUuid : String
    , carousels : Dict String (Carousel.Model msg)
    , googleMapUrl : Maybe String
    , gmSizeSelector : Select.Model
    , googleMapMeta : Maybe GoogleMapMeta
    , outMsg : Msg -> msg
    }


type DisplayMode
    = Preview
    | EditMarkdown
    | EditPicRow
    | EditCarousel String
    | EditGoogleMap


type Msg
    = EditItem Int
    | DeleteItem Int
    | SwapUp Int
    | SwapDown Int
    | NewMarkdown
    | NewPicRow
    | NewCarousel
    | NewGoogleMap
    | MarkdownEditorMsg MarkdownEditor.Msg
    | ImageControllerMsg ImageController.Msg
    | GotGenericPageContent (Result Http.Error GenericPageContent)
    | CarouselMsg String Carousel.Msg
    | GoogleMapUrlInput String
    | GmSize ( Int, Int )
    | GmSizeSelectorMsg Select.Msg
    | GMGoBack
    | GMSaveAndQuit
    | GenericPageSaved (Result Http.Error ())
    | DownloadDoc String
    | NoOp


init : (Msg -> msg) -> String -> ( String, Seed ) -> ( Model msg, Cmd msg )
init outMsg wd ( uuid, seed ) =
    let
        ( imageController, imgCtrlCmd ) =
            ImageController.init
                wd
                ImageController.RowMode
                (outMsg << ImageControllerMsg)
    in
    ( { workingDirectory = wd
      , content =
            Dict.empty
      , markdownEditor =
            MarkdownEditor.init
                French
                (outMsg << MarkdownEditorMsg)
      , imageController = imageController
      , displayMode = Preview
      , selectedItem = Nothing
      , currentSeed = seed
      , currentUuid = uuid
      , carousels = Dict.empty
      , googleMapUrl = Nothing
      , gmSizeSelector = Select.init
      , googleMapMeta = Nothing
      , outMsg = outMsg
      }
    , Cmd.batch
        [ imgCtrlCmd
        , getGenericPageContent (outMsg << GotGenericPageContent) wd
        ]
    )


subscriptions model =
    Sub.batch
        [ ImageController.subscriptions model.imageController ]


update : { a | logInfo : LogInfo, width : Int } -> Msg -> Model msg -> ( Model msg, Cmd msg )
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

                        GoogleMap gm ->
                            ( { model
                                | selectedItem = Just id
                                , displayMode = EditGoogleMap
                                , googleMapMeta = Just gm
                                , googleMapUrl = Just gm.url
                              }
                            , Cmd.none
                            )

                        Carousel cId pics ->
                            let
                                ( imgCtrl, imgCtrlCmd ) =
                                    ImageController.load
                                        config.logInfo
                                        model.imageController
                                        pics
                            in
                            ( { model
                                | imageController = imgCtrl
                                , displayMode = EditCarousel cId
                                , selectedItem = Just id
                              }
                            , imgCtrlCmd
                            )

                Nothing ->
                    ( model, Cmd.none )

        DeleteItem id ->
            let
                newModel =
                    { model | content = remove id model.content }
            in
            ( newModel
            , saveGenericPage config.logInfo newModel
            )

        SwapUp id ->
            let
                newContent =
                    Dict.values model.content
                        |> swapAt id (id - 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList

                newModel =
                    { model | content = newContent }
            in
            ( newModel
            , saveGenericPage config.logInfo newModel
            )

        SwapDown id ->
            let
                newContent =
                    Dict.values model.content
                        |> swapAt id (id + 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList

                newModel =
                    { model | content = newContent }
            in
            ( newModel
            , saveGenericPage config.logInfo newModel
            )

        GotGenericPageContent res ->
            case res of
                Ok content ->
                    ( { model
                        | content =
                            List.indexedMap Tuple.pair content
                                |> Dict.fromList
                        , carousels =
                            initCarousels content
                                (\id -> model.outMsg << CarouselMsg id)
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

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

        NewCarousel ->
            let
                ( imgCtrl, imgCtrlCmd ) =
                    ImageController.load
                        config.logInfo
                        model.imageController
                        []

                ( uuid, seed ) =
                    step Uuid.stringGenerator model.currentSeed
            in
            ( { model
                | imageController = imgCtrl
                , displayMode = EditCarousel model.currentUuid
                , currentUuid = uuid
                , currentSeed = seed
                , selectedItem = Just (nextId model.content)
              }
            , imgCtrlCmd
            )

        NewGoogleMap ->
            ( { model
                | displayMode = EditGoogleMap
                , googleMapUrl = Nothing
                , gmSizeSelector = Select.init
                , googleMapMeta = Nothing
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
                            let
                                newModel =
                                    { model
                                        | markdownEditor = newEditor
                                        , content =
                                            Dict.insert
                                                id
                                                (MarkdownContent data)
                                                model.content
                                        , displayMode = Preview
                                    }
                            in
                            ( newModel
                            , saveGenericPage config.logInfo newModel
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
                            let
                                newModel =
                                    { model
                                        | imageController = newImgCtrl
                                        , content =
                                            case model.displayMode of
                                                EditCarousel cId ->
                                                    Dict.insert
                                                        id
                                                        (Carousel cId data)
                                                        model.content

                                                _ ->
                                                    Dict.insert
                                                        id
                                                        (ImageRow data)
                                                        model.content
                                        , carousels =
                                            case model.displayMode of
                                                EditCarousel cId ->
                                                    Dict.insert cId
                                                        (Carousel.init (List.map (\i -> awsUrl ++ i.url) data)
                                                            (model.outMsg << CarouselMsg cId)
                                                        )
                                                        model.carousels

                                                _ ->
                                                    model.carousels
                                        , displayMode = Preview
                                    }
                            in
                            ( newModel
                            , saveGenericPage config.logInfo newModel
                            )

        CarouselMsg id carMsg ->
            case Dict.get id model.carousels of
                Just carousel ->
                    ( { model
                        | carousels =
                            Dict.insert id
                                (Carousel.update
                                    { maxWidth = config.width }
                                    carMsg
                                    carousel
                                )
                                model.carousels
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GoogleMapUrlInput s ->
            ( { model
                | googleMapUrl =
                    if s == "" then
                        Nothing

                    else
                        Just s
                , googleMapMeta = parseHtml s
              }
            , Cmd.none
            )

        GmSize ( w, h ) ->
            case model.googleMapMeta of
                Just gmm ->
                    ( { model
                        | googleMapMeta =
                            Just { gmm | width = w, height = h }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GmSizeSelectorMsg selMsg ->
            ( { model
                | gmSizeSelector =
                    Select.update selMsg model.gmSizeSelector
              }
            , Cmd.none
            )

        GMGoBack ->
            ( { model | displayMode = Preview }, Cmd.none )

        GMSaveAndQuit ->
            case ( model.selectedItem, model.googleMapMeta ) of
                ( Just id, Just gm ) ->
                    let
                        newModel =
                            { model
                                | content =
                                    Dict.insert
                                        id
                                        (GoogleMap gm)
                                        model.content
                                , displayMode = Preview
                            }
                    in
                    ( newModel
                    , saveGenericPage config.logInfo newModel
                    )

                _ ->
                    ( { model
                        | displayMode = Preview
                      }
                    , Cmd.none
                    )

        GenericPageSaved res ->
            case res of
                Ok _ ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DownloadDoc url ->
            ( model, Download.url url )

        NoOp ->
            ( model, Cmd.none )


type alias ViewConfig =
    { lang : Lang
    , width : Int
    , logInfo : LogInfo
    , documents : Dict String String
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    case model.displayMode of
        Preview ->
            previewView config model

        EditMarkdown ->
            MarkdownEditor.view config model.markdownEditor

        EditPicRow ->
            ImageController.view config model.imageController

        EditCarousel cId ->
            ImageController.view config model.imageController

        EditGoogleMap ->
            googleMapEditorView config model


previewView : ViewConfig -> Model msg -> Element msg
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
                { onPress = Just (model.outMsg NewMarkdown)
                , label =
                    textM config.lang
                        { en = "New article"
                        , fr = "Nouvel article"
                        }
                }
            , Input.button
                (buttonStyle True)
                { onPress = Just (model.outMsg NewPicRow)
                , label =
                    textM config.lang
                        { en = "Add images"
                        , fr = "Ajouter images"
                        }
                }
            , Input.button
                (buttonStyle True)
                { onPress = Just (model.outMsg NewCarousel)
                , label =
                    textM config.lang
                        { en = "Add carousel"
                        , fr = "Ajouter carousel"
                        }
                }
            , Input.button
                (buttonStyle True)
                { onPress = Just (model.outMsg NewGoogleMap)
                , label =
                    textM config.lang
                        { en = "Add map"
                        , fr = "Ajouter carte"
                        }
                }
            ]
        , if model.content == Dict.empty then
            Element.none

          else
            Dict.map (editableItem config model) model.content
                |> Dict.values
                |> column
                    [ Background.color grey
                    , width fill
                    , centerX
                    , padding 10
                    , spacing 20
                    ]
        ]


editableItem : ViewConfig -> Model msg -> Int -> GenericPageItem -> Element msg
editableItem config model id item =
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
        [ genericPageItemView
            { lang = config.lang
            , width = config.width
            , carousels = model.carousels
            , downloadHandler = model.outMsg << DownloadDoc
            }
            item
        , itemControlView config model id
        ]


itemControlView : ViewConfig -> Model msg -> Int -> Element msg
itemControlView config model id =
    Element.map model.outMsg <|
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


googleMapEditorView : ViewConfig -> Model msg -> Element msg
googleMapEditorView config model =
    let
        sizesDict =
            Dict.fromList
                [ ( ( 400, 300 )
                  , MultLangStr
                        "Small"
                        "Petit"
                  )
                , ( ( 600, 450 )
                  , MultLangStr
                        "Medium"
                        "Moyen"
                  )
                , ( ( 800, 600 )
                  , MultLangStr
                        "Big"
                        "Grand"
                  )
                ]
    in
    Element.map model.outMsg <|
        column
            [ width fill
            , height fill
            , Background.color lightGrey
            , spacing 15
            , centerX
            , padding 40
            ]
            [ row
                [ spacing 15
                , centerX
                ]
                [ Input.text
                    textInputStyle
                    { onChange = GoogleMapUrlInput
                    , text =
                        model.googleMapUrl
                            |> Maybe.withDefault ""
                    , placeholder =
                        Just <|
                            Input.placeholder
                                []
                                (textM config.lang
                                    (MultLangStr "GoogleMap embed string"
                                        "Code Google map"
                                    )
                                )
                    , label = Input.labelHidden ""
                    }
                , Select.view
                    { outMsg = GmSizeSelectorMsg
                    , items =
                        [ ( strM config.lang
                                (MultLangStr
                                    "Small"
                                    "Petit"
                                )
                          , GmSize ( 400, 300 )
                          )
                        , ( strM config.lang
                                (MultLangStr
                                    "Medium"
                                    "Moyen"
                                )
                          , GmSize ( 600, 450 )
                          )
                        , ( strM config.lang
                                (MultLangStr
                                    "Large"
                                    "Grand"
                                )
                          , GmSize ( 800, 600 )
                          )
                        ]
                    , selected =
                        model.googleMapMeta
                            |> Maybe.map
                                (\meta -> ( meta.width, meta.height ))
                            |> Maybe.andThen (\s -> Dict.get s sizesDict)
                            |> Maybe.map (strM config.lang)
                    , placeholder =
                        Just "-"
                    , label =
                        Just <|
                            Input.labelLeft
                                [ centerY
                                , paddingEach { sides | right = 10 }
                                ]
                                (textM config.lang
                                    (MultLangStr
                                        "Size"
                                        "Format"
                                    )
                                )
                    }
                    model.gmSizeSelector
                ]
            , case model.googleMapMeta of
                Just gm ->
                    googleMapIframe gm

                Nothing ->
                    Element.none
            , row
                [ centerX
                , spacing 10
                ]
                [ Input.button
                    (buttonStyle True)
                    { onPress = Just GMGoBack
                    , label =
                        textM config.lang (MultLangStr "Back" "Retour")
                    }
                , Input.button
                    (buttonStyle (model.googleMapMeta /= Nothing))
                    { onPress =
                        if model.googleMapMeta /= Nothing then
                            Just GMSaveAndQuit

                        else
                            Nothing
                    , label =
                        textM config.lang (MultLangStr "Save and quit" "Valider")
                    }
                ]
            ]



-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------


saveGenericPage : LogInfo -> Model msg -> Cmd msg
saveGenericPage logInfo model =
    let
        body =
            Dict.values model.content
                |> (\c ->
                        E.object
                            [ ( "name", E.string model.workingDirectory )
                            , ( "content", encodeGenericPage c )
                            ]
                            |> Http.jsonBody
                   )
    in
    securePost logInfo
        { url = "api/restricted/pagesdata"
        , body = body
        , expect =
            Http.expectWhatever (model.outMsg << GenericPageSaved)
        }


encodeGenericPage : GenericPageContent -> E.Value
encodeGenericPage fpi =
    E.list encodeGenericPageItem fpi


encodeGenericPageItem : GenericPageItem -> E.Value
encodeGenericPageItem fpi =
    case fpi of
        MarkdownContent mls ->
            E.object
                [ ( "MarkdownContent", encodeMls mls ) ]

        ImageRow imgs ->
            E.object
                [ ( "ImageRow", E.list encodeImageMeta imgs ) ]

        GoogleMap gm ->
            E.object
                [ ( "GoogleMap", encodeGoogleMapMeta gm ) ]

        Carousel id imgs ->
            E.object
                [ ( "Carousel"
                  , E.object
                        [ ( "id", E.string id )
                        , ( "imgs", E.list encodeImageMeta imgs )
                        ]
                  )
                ]


encodeGoogleMapMeta : GoogleMapMeta -> E.Value
encodeGoogleMapMeta gm =
    E.object
        [ ( "frameborder", E.bool gm.frameBorder )
        , ( "pb", E.string gm.pb )
        , ( "height", E.int gm.height )
        , ( "width", E.int gm.width )
        , ( "url", E.string gm.url )
        ]
