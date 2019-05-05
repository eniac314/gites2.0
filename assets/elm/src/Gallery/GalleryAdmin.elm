module Gallery.GalleryAdmin exposing (..)

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
import Gallery.Gallery as Gallery
import Gallery.GalleryShared exposing (..)
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
import Style.Icons as Icons
import Style.Palette exposing (..)


type alias Model msg =
    { galleries : Dict String GalleryMeta
    , markdownEditor : MarkdownEditor.Model msg
    , gallery : Maybe (Gallery.Model msg)
    , controllers : Dict String (ImageController.Model msg)
    , mainArticle : Maybe MultLangStr
    , titleInputEn : Maybe String
    , titleInputFr : Maybe String
    , displayMode : DisplayMode
    , outMsg : Msg -> msg
    }


type DisplayMode
    = DisplayHome
    | EditGalleryMeta String
    | EditGalleryImgs String
    | EditMarkdown EditMarkdownMode String


type EditMarkdownMode
    = EditArticleMode
    | EditHeaderMode
    | EditMainArticleMode


type Msg
    = NewGalleryEnTitle String
    | NewGalleryFrTitle String
    | ConfirmTitleChange String
    | MakeNewGallery
    | EditMainArticle
    | SwapLeft Int
    | SwapRight Int
    | EditMeta String
    | EditHeader String
    | EditArticle String
    | EditImgs String
    | DeleteGallery String
    | GalleryDeleted (Result Http.Error ())
    | MarkdownEditorMsg MarkdownEditor.Msg
    | ImageControllerMsg String ImageController.Msg
    | GalleryMsg Gallery.Msg
    | GotGalleryMetas (Result Http.Error DetailsPage)
    | Save
    | Saved (Result Http.Error ())
    | SetDisplayMode DisplayMode
    | DownloadDoc String
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { galleries = Dict.empty
      , markdownEditor =
            MarkdownEditor.init
                French
                (outMsg << MarkdownEditorMsg)
      , gallery = Nothing
      , controllers = Dict.empty
      , mainArticle = Nothing
      , titleInputEn = Nothing
      , titleInputFr = Nothing
      , displayMode = DisplayHome
      , outMsg = outMsg
      }
    , Cmd.batch
        [ getDetailsPage (outMsg << GotGalleryMetas)
        ]
    )


subscriptions : Model msg -> Sub msg
subscriptions model =
    Sub.batch
        ([ Maybe.map Gallery.subscriptions model.gallery
            |> Maybe.withDefault Sub.none
         ]
            ++ (Dict.map
                    (\wd ic ->
                        ImageController.subscriptions ic
                    )
                    model.controllers
                    |> Dict.values
               )
        )


type alias Config a =
    { a
        | width : Int
        , logInfo : LogInfo
    }


update : Config a -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        NewGalleryEnTitle title ->
            ( { model
                | titleInputEn =
                    if title == "" then
                        Nothing
                    else
                        Just title
              }
            , Cmd.none
            )

        NewGalleryFrTitle title ->
            ( { model
                | titleInputFr =
                    if title == "" then
                        Nothing
                    else
                        Just title
              }
            , Cmd.none
            )

        EditMainArticle ->
            ( { model
                | markdownEditor =
                    MarkdownEditor.load
                        model.markdownEditor
                        model.mainArticle
                , displayMode = EditMarkdown EditMainArticleMode ""
              }
            , Cmd.none
            )

        SwapLeft id ->
            let
                newGalleries =
                    Dict.toList model.galleries
                        |> List.sortBy (.ordering << Tuple.second)
                        |> swapAt id (id - 1)
                        |> List.indexedMap (\i ( k, v ) -> ( k, { v | ordering = i } ))
                        |> Dict.fromList

                newModel =
                    { model | galleries = newGalleries }
            in
            ( newModel
            , saveDetailsPage config.logInfo newModel
            )

        SwapRight id ->
            let
                newGalleries =
                    Dict.toList model.galleries
                        |> List.sortBy (.ordering << Tuple.second)
                        |> swapAt id (id + 1)
                        |> List.indexedMap (\i ( k, v ) -> ( k, { v | ordering = i } ))
                        |> Dict.fromList

                newModel =
                    { model | galleries = newGalleries }
            in
            ( newModel
            , saveDetailsPage config.logInfo newModel
            )

        ConfirmTitleChange key ->
            case ( model.titleInputFr, model.titleInputEn, Dict.get key model.galleries ) of
                ( Just fr, Just en, Just g ) ->
                    let
                        newModel =
                            { model
                                | galleries =
                                    Dict.insert
                                        key
                                        { g | title = MultLangStr en fr }
                                        model.galleries
                            }
                                |> reloadGallery key
                    in
                    ( newModel
                    , saveDetailsPage config.logInfo newModel
                    )

                _ ->
                    ( model, Cmd.none )

        MakeNewGallery ->
            case ( model.titleInputEn, model.titleInputFr ) of
                ( Just enTitle, Just frTitle ) ->
                    let
                        newGallery =
                            { title =
                                MultLangStr enTitle frTitle
                            , key = enTitle
                            , ordering = Dict.size model.galleries
                            , titleImg = Nothing
                            , header = Nothing
                            , article = Nothing
                            , album = []
                            }

                        ( c, cmd ) =
                            ImageController.init
                                enTitle
                                ImageController.GalleryMode
                                (model.outMsg << ImageControllerMsg enTitle)

                        newModel =
                            { model
                                | galleries =
                                    Dict.insert enTitle newGallery model.galleries
                                , controllers =
                                    Dict.insert enTitle c model.controllers
                                , displayMode =
                                    EditGalleryMeta enTitle
                            }
                                |> reloadGallery enTitle
                    in
                    ( newModel
                    , saveDetailsPage config.logInfo newModel
                    )

                _ ->
                    ( model, Cmd.none )

        EditMeta title ->
            ( { model
                | displayMode = EditGalleryMeta title
                , titleInputFr =
                    Dict.get title model.galleries
                        |> Maybe.map (.fr << .title)
                , titleInputEn =
                    Dict.get title model.galleries
                        |> Maybe.map (.en << .title)
              }
                |> reloadGallery title
            , Cmd.none
            )

        EditHeader title ->
            case Dict.get title model.galleries of
                Just { header } ->
                    ( { model
                        | markdownEditor =
                            MarkdownEditor.load
                                model.markdownEditor
                                header
                        , displayMode = EditMarkdown EditHeaderMode title
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditArticle title ->
            case Dict.get title model.galleries of
                Just { article } ->
                    ( { model
                        | markdownEditor =
                            MarkdownEditor.load
                                model.markdownEditor
                                article
                        , displayMode = EditMarkdown EditArticleMode title
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditImgs title ->
            case
                ( Dict.get title model.galleries
                , Dict.get title model.controllers
                )
            of
                ( Just g, Just c ) ->
                    let
                        ( loadedController, cmd ) =
                            ImageController.load config.logInfo c g.album
                    in
                    ( { model
                        | controllers = Dict.insert title loadedController model.controllers
                        , displayMode = EditGalleryImgs title
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteGallery title ->
            case Dict.get title model.controllers of
                Just controller ->
                    let
                        newModel =
                            { model
                                | displayMode = DisplayHome
                                , galleries = Dict.remove title model.galleries
                                , controllers = Dict.remove title model.controllers
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ ImageController.deleteWorkingDirectory config.logInfo controller
                        , saveDetailsPage config.logInfo newModel
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GalleryDeleted res ->
            ( model, Cmd.none )

        MarkdownEditorMsg markdownEditorMsg ->
            let
                ( newEditor, mbPluginRes ) =
                    MarkdownEditor.update markdownEditorMsg model.markdownEditor
            in
            case model.displayMode of
                EditMarkdown mode title ->
                    case mbPluginRes of
                        Nothing ->
                            ( { model | markdownEditor = newEditor }
                            , Cmd.none
                            )

                        Just PluginQuit ->
                            ( { model
                                | markdownEditor = newEditor
                                , displayMode =
                                    if mode == EditMainArticleMode then
                                        DisplayHome
                                    else
                                        EditGalleryMeta title
                              }
                                |> reloadGallery title
                            , Cmd.none
                            )

                        Just (PluginData data) ->
                            let
                                newModel =
                                    if mode == EditMainArticleMode then
                                        { model
                                            | mainArticle = Just data
                                            , displayMode = DisplayHome
                                        }
                                    else
                                        { model
                                            | markdownEditor = newEditor
                                            , galleries =
                                                Dict.update
                                                    title
                                                    (\mbG ->
                                                        case mbG of
                                                            Nothing ->
                                                                Nothing

                                                            Just g ->
                                                                case mode of
                                                                    EditArticleMode ->
                                                                        Just { g | article = Just data }

                                                                    EditHeaderMode ->
                                                                        Just { g | header = Just data }

                                                                    _ ->
                                                                        Just g
                                                    )
                                                    model.galleries
                                            , displayMode = EditGalleryMeta title
                                        }
                                            |> reloadGallery title
                            in
                            ( newModel
                            , saveDetailsPage config.logInfo newModel
                            )

                _ ->
                    ( model, Cmd.none )

        ImageControllerMsg title imageControllerMsg ->
            case Dict.get title model.controllers of
                Just imgController ->
                    let
                        ( newImgCtrl, cmd, mbPluginRes ) =
                            ImageController.update
                                config
                                imageControllerMsg
                                imgController
                    in
                    case mbPluginRes of
                        Nothing ->
                            ( { model
                                | controllers =
                                    Dict.insert
                                        title
                                        newImgCtrl
                                        model.controllers
                              }
                            , cmd
                            )

                        Just PluginQuit ->
                            ( { model
                                | controllers =
                                    Dict.insert
                                        title
                                        newImgCtrl
                                        model.controllers
                                , displayMode = EditGalleryMeta title
                              }
                                |> reloadGallery title
                            , cmd
                            )

                        Just (PluginData data) ->
                            let
                                newModel =
                                    { model
                                        | controllers =
                                            Dict.insert
                                                title
                                                newImgCtrl
                                                model.controllers
                                        , galleries =
                                            Dict.update
                                                title
                                                (\mbG ->
                                                    case mbG of
                                                        Nothing ->
                                                            Nothing

                                                        Just g ->
                                                            Just { g | album = data }
                                                )
                                                model.galleries
                                        , displayMode = EditGalleryMeta title
                                    }
                                        |> reloadGallery title
                            in
                            ( newModel
                            , saveDetailsPage config.logInfo newModel
                            )

                _ ->
                    ( model, Cmd.none )

        GalleryMsg gMsg ->
            case model.gallery of
                Just g ->
                    let
                        ( gallery, cmd ) =
                            Gallery.update config gMsg g
                    in
                    ( { model | gallery = Just gallery }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        GotGalleryMetas res ->
            case res of
                Ok { mainArticle, galleries } ->
                    let
                        ( controllers, cmds ) =
                            Dict.foldr
                                (\t g ( ctrls, cs ) ->
                                    let
                                        ( c, cmd ) =
                                            ImageController.init
                                                t
                                                ImageController.GalleryMode
                                                (model.outMsg << ImageControllerMsg t)
                                    in
                                    ( Dict.insert t c ctrls, cmd :: cs )
                                )
                                ( Dict.empty, [] )
                                galleries
                    in
                    ( { model
                        | galleries = galleries
                        , controllers = controllers
                        , mainArticle = mainArticle
                      }
                    , Cmd.batch cmds
                    )

                _ ->
                    ( model, Cmd.none )

        Save ->
            ( model, Cmd.none )

        Saved res ->
            ( model, Cmd.none )

        SetDisplayMode dm ->
            ( { model
                | displayMode = dm
                , titleInputEn =
                    Maybe.map .en (extractTitle model dm)
                , titleInputFr =
                    Maybe.map .fr (extractTitle model dm)
              }
            , Cmd.none
            )

        DownloadDoc url ->
            ( model, Download.url url )

        NoOp ->
            ( model, Cmd.none )


reloadGallery title model =
    { model
        | gallery =
            Dict.get title model.galleries
                |> Maybe.map
                    (\gm ->
                        Gallery.init gm
                            (model.outMsg << GalleryMsg)
                    )
    }


extractTitle model dm =
    let
        go title =
            Dict.get title model.galleries
                |> Maybe.map .title
    in
    case dm of
        DisplayHome ->
            Nothing

        EditGalleryMeta t ->
            go t

        EditGalleryImgs t ->
            go t

        EditMarkdown _ t ->
            go t


extractKey dm =
    case dm of
        EditGalleryMeta t ->
            Just t

        EditGalleryImgs t ->
            Just t

        EditMarkdown _ t ->
            Just t

        _ ->
            Nothing



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


type alias ViewConfig =
    { lang : Lang
    , width : Int
    , logInfo : LogInfo
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    el
        [ width fill
        , height fill
        , paddingEach { top = 45, right = 45, bottom = 45, left = 45 }
        , spacing 30
        , Background.color lightGrey
        ]
        (case model.displayMode of
            DisplayHome ->
                Element.map model.outMsg <|
                    homeView config model

            EditGalleryMeta title ->
                galleryMetaView config model title

            EditGalleryImgs title ->
                galleryImgView config model title

            EditMarkdown _ title ->
                MarkdownEditor.view config model.markdownEditor
        )


homeView : ViewConfig -> Model msg -> Element Msg
homeView config model =
    let
        canMakeNewGallery =
            (model.titleInputEn /= Nothing)
                && (model.titleInputFr /= Nothing)

        controls id =
            row
                [ centerX
                , spacing 10
                , padding 10
                ]
                [ Input.button
                    iconsStyle
                    { onPress = Just <| SwapLeft id
                    , label =
                        Icons.arrowLeft
                            (Icons.defOptions
                                |> Icons.color black
                                |> Icons.size 25
                            )
                    }
                , Input.button
                    iconsStyle
                    { onPress = Just <| SwapRight id
                    , label =
                        Icons.arrowRight
                            (Icons.defOptions
                                |> Icons.color black
                                |> Icons.size 25
                            )
                    }
                ]

        gallerySelectorView gm =
            column
                []
                [ imgBlockView config.lang EditMeta gm
                , controls gm.ordering
                ]
    in
    column
        [ spacing 15
        , width fill
        ]
        [ column
            [ spacing 15
            , centerX
            , padding 15
            , Background.color white
            , Border.rounded 5
            , Border.width 1
            , Border.color grey
            ]
            [ row
                [ spacing 15
                ]
                [ Input.text
                    textInputStyle
                    { onChange = NewGalleryEnTitle
                    , text =
                        model.titleInputEn
                            |> Maybe.withDefault ""
                    , placeholder =
                        Just <|
                            Input.placeholder []
                                (textM config.lang
                                    (MultLangStr "Page name - (english)"
                                        "Nom de la page - (Anglais)"
                                    )
                                )
                    , label =
                        Input.labelHidden ""
                    }
                , Input.text
                    textInputStyle
                    { onChange = NewGalleryFrTitle
                    , text =
                        model.titleInputFr
                            |> Maybe.withDefault ""
                    , placeholder =
                        Just <|
                            Input.placeholder []
                                (textM config.lang
                                    (MultLangStr "Page name - (french)"
                                        "Nom de la page - (Français)"
                                    )
                                )
                    , label =
                        Input.labelHidden ""
                    }
                , Input.button
                    (buttonStyle canMakeNewGallery)
                    { onPress =
                        if canMakeNewGallery then
                            Just MakeNewGallery
                        else
                            Nothing
                    , label =
                        textM config.lang
                            (MultLangStr "New page" "Nouvelle page")
                    }
                ]
            , Input.button
                (buttonStyle True)
                { onPress =
                    Just EditMainArticle
                , label =
                    textM config.lang
                        (MultLangStr "Edit main article" "Modifier présentation")
                }
            ]
        , column
            [ centerX ]
            ([ case model.mainArticle of
                Just a ->
                    MarkdownParser.renderMarkdown
                        (strM config.lang a)
                        DownloadDoc

                Nothing ->
                    Element.none
             ]
                ++ chunkedRows
                    (min config.width 1000)
                    (bestFit 200)
                    (Dict.map
                        (\k v ->
                            let
                                defTitleImg =
                                    case v.titleImg of
                                        Just url ->
                                            url

                                        Nothing ->
                                            List.head v.album
                                                |> Maybe.map .url
                                                |> Maybe.withDefault ""
                            in
                            { v | titleImg = Just defTitleImg }
                        )
                        model.galleries
                        |> Dict.values
                        |> List.sortBy .ordering
                        |> List.map gallerySelectorView
                    )
            )
        ]


galleryMetaView : ViewConfig -> Model msg -> String -> Element msg
galleryMetaView config model title =
    case Dict.get title model.galleries of
        Just g ->
            let
                defTitleImg =
                    case g.titleImg of
                        Just url ->
                            url

                        Nothing ->
                            List.head g.album
                                |> Maybe.map .url
                                |> Maybe.withDefault ""
            in
            column
                [ spacing 15
                , width fill
                ]
                [ row
                    [ centerX
                    , spacing 15
                    ]
                    [ el
                        [ centerX ]
                        (imgBlockView config.lang (\_ -> model.outMsg NoOp) { g | titleImg = Just defTitleImg })
                    , column
                        [ spacing 15
                        ]
                        [ Input.text
                            textInputStyle
                            { onChange = model.outMsg << NewGalleryEnTitle
                            , text =
                                model.titleInputEn
                                    |> Maybe.withDefault ""
                            , placeholder =
                                Just <|
                                    Input.placeholder []
                                        (textM config.lang
                                            (MultLangStr "Page name - (english)"
                                                "Nom de la page - (Anglais)"
                                            )
                                        )
                            , label =
                                Input.labelHidden ""
                            }
                        , Input.text
                            textInputStyle
                            { onChange = model.outMsg << NewGalleryFrTitle
                            , text =
                                model.titleInputFr
                                    |> Maybe.withDefault ""
                            , placeholder =
                                Just <|
                                    Input.placeholder []
                                        (textM config.lang
                                            (MultLangStr "Page name - (french)"
                                                "Nom de la page - (Français)"
                                            )
                                        )
                            , label =
                                Input.labelHidden ""
                            }
                        , Input.button
                            (buttonStyle True)
                            { onPress =
                                Just (model.outMsg <| ConfirmTitleChange title)
                            , label =
                                textM config.lang
                                    (MultLangStr "Change title" "Modifier titre")
                            }
                        ]
                    ]
                , row
                    [ centerX
                    , spacing 15
                    ]
                    [ Input.button
                        (buttonStyle True)
                        { onPress =
                            Just (model.outMsg <| EditHeader title)
                        , label =
                            textM config.lang
                                (MultLangStr "Edit header" "Modifier en-tête")
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress =
                            Just (model.outMsg <| EditArticle title)
                        , label =
                            textM config.lang
                                (MultLangStr "Edit article" "Modifier article")
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress =
                            Just (model.outMsg <| EditImgs title)
                        , label =
                            textM config.lang
                                (MultLangStr "Edit Album" "Modifier album")
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress =
                            Just (model.outMsg <| DeleteGallery title)
                        , label =
                            textM config.lang
                                (MultLangStr "Delete page" "Supprimer page")
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress =
                            Just (model.outMsg <| SetDisplayMode DisplayHome)
                        , label =
                            textM config.lang
                                (MultLangStr "Go back" "Retour")
                        }
                    ]
                , column
                    [ padding 15
                    , Background.color white
                    , Border.color grey
                    , Border.rounded 5
                    , width (px (min config.width 1000))
                    , centerX
                    ]
                    [ case g.header of
                        Just h ->
                            MarkdownParser.renderMarkdown
                                (strM config.lang h)
                                (model.outMsg << DownloadDoc)

                        Nothing ->
                            Element.none
                    , case model.gallery of
                        Just gallery ->
                            Gallery.view config gallery

                        _ ->
                            Element.none
                    , case g.article of
                        Just a ->
                            MarkdownParser.renderMarkdown
                                (strM config.lang a)
                                (model.outMsg << DownloadDoc)

                        Nothing ->
                            Element.none
                    ]
                ]

        _ ->
            Element.none


galleryImgView : ViewConfig -> Model msg -> String -> Element msg
galleryImgView config model title =
    case Dict.get title model.controllers of
        Just imgController ->
            ImageController.view config imgController

        _ ->
            Element.none



-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------


saveDetailsPage : LogInfo -> Model msg -> Cmd msg
saveDetailsPage logInfo model =
    let
        body =
            E.object
                [ ( "name", E.string "details" )
                , ( "content", encodePage model )
                ]
                |> Http.jsonBody
    in
    securePost logInfo
        { url = "api/restricted/pagesdata"
        , body = body
        , expect =
            Http.expectWhatever (model.outMsg << Saved)
        }


encodePage model =
    E.object
        [ ( "mainArticle"
          , Maybe.map encodeMls model.mainArticle
                |> Maybe.withDefault E.null
          )
        , ( "galleries", encodeGalleryMetas model.galleries )
        ]


encodeGalleryMetas : Dict String GalleryMeta -> E.Value
encodeGalleryMetas galleries =
    Dict.values galleries
        |> E.list encodeGalleryMeta


encodeGalleryMeta : GalleryMeta -> E.Value
encodeGalleryMeta { key, ordering, title, titleImg, header, article, album } =
    E.object
        [ ( "key", E.string key )
        , ( "ordering", E.int ordering )
        , ( "title", encodeMls title )
        , ( "titleImg"
          , Maybe.map E.string titleImg
                |> Maybe.withDefault E.null
          )
        , ( "header"
          , Maybe.map encodeMls header
                |> Maybe.withDefault E.null
          )
        , ( "article"
          , Maybe.map encodeMls article
                |> Maybe.withDefault E.null
          )
        , ( "album", E.list encodeImageMeta album )
        ]
