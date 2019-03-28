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
import Gallery.Gallery as Gallery
import Gallery.GalleryShared exposing (..)
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.ImageController as ImageController
import Internals.MarkdownEditor as MarkdownEditor
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Palette exposing (..)


type alias Model msg =
    { galleries : Dict String GalleryMeta
    , markdownEditor : MarkdownEditor.Model msg
    , gallery : Maybe (Gallery.Model msg)
    , controllers : Dict String (ImageController.Model msg)
    , newGalleryInputEn : Maybe String
    , newGalleryInputFr : Maybe String
    , displayMode : DisplayMode
    , outMsg : Msg -> msg
    }


type DisplayMode
    = DisplayHome
    | EditGalleryMeta String
    | EditGalleryImgs String
    | EditGalleryArticle String


type Msg
    = NewGalleryEnTitle String
    | NewGalleryFrTitle String
    | MakeNewGallery
    | EditMeta String
    | EditArticle String
    | EditImgs String
    | DeleteGallery String
    | GalleryDeleted (Result Http.Error ())
    | MarkdownEditorMsg MarkdownEditor.Msg
    | ImageControllerMsg String ImageController.Msg
    | GalleryMsg Gallery.Msg
    | GotGalleryMetas (Result Http.Error (Dict String GalleryMeta))
    | Save
    | Saved (Result Http.Error ())
    | SetDisplayMode DisplayMode
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
      , newGalleryInputEn = Nothing
      , newGalleryInputFr = Nothing
      , displayMode = DisplayHome
      , outMsg = outMsg
      }
    , Cmd.batch
        [ getGalleryMetas (outMsg << GotGalleryMetas)
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
                | newGalleryInputEn =
                    if title == "" then
                        Nothing
                    else
                        Just title
              }
            , Cmd.none
            )

        NewGalleryFrTitle title ->
            ( { model
                | newGalleryInputFr =
                    if title == "" then
                        Nothing
                    else
                        Just title
              }
            , Cmd.none
            )

        MakeNewGallery ->
            case ( model.newGalleryInputEn, model.newGalleryInputFr ) of
                ( Just enTitle, Just frTitle ) ->
                    let
                        newGallery =
                            { title =
                                MultLangStr enTitle frTitle
                            , titleImg = Nothing
                            , article = Nothing
                            , album = []
                            }

                        ( c, cmd ) =
                            ImageController.init
                                enTitle
                                ImageController.GalleryMode
                                (model.outMsg << ImageControllerMsg enTitle)
                    in
                    ( { model
                        | galleries =
                            Dict.insert enTitle newGallery model.galleries
                        , controllers =
                            Dict.insert enTitle c model.controllers
                        , displayMode =
                            EditGalleryMeta enTitle
                      }
                        |> reloadGallery enTitle
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        EditMeta title ->
            ( { model
                | displayMode = EditGalleryMeta title
              }
                |> reloadGallery title
            , Cmd.none
            )

        EditArticle title ->
            case Dict.get title model.galleries of
                Just { article } ->
                    ( { model
                        | markdownEditor =
                            MarkdownEditor.load
                                model.markdownEditor
                                article
                        , displayMode = EditGalleryArticle title
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
                    ( { model | displayMode = DisplayHome }
                    , Cmd.batch
                        [ ImageController.deleteWorkingDirectory config.logInfo controller
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
                EditGalleryArticle title ->
                    case mbPluginRes of
                        Nothing ->
                            ( { model | markdownEditor = newEditor }
                            , Cmd.none
                            )

                        Just PluginQuit ->
                            ( { model
                                | markdownEditor = newEditor
                                , displayMode = EditGalleryMeta title
                              }
                                |> reloadGallery title
                            , Cmd.none
                            )

                        Just (PluginData data) ->
                            ( { model
                                | markdownEditor = newEditor
                                , galleries =
                                    Dict.update
                                        title
                                        (\mbG ->
                                            case mbG of
                                                Nothing ->
                                                    Nothing

                                                Just g ->
                                                    Just { g | article = Just data }
                                        )
                                        model.galleries
                                , displayMode = EditGalleryMeta title
                              }
                                |> reloadGallery title
                            , Cmd.none
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
                            ( { model
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
                            , cmd
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
                Ok galleries ->
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
            ( { model | displayMode = dm }
            , Cmd.none
            )

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

            EditGalleryArticle title ->
                galleryArticleView config model title
        )


homeView : ViewConfig -> Model msg -> Element Msg
homeView config model =
    let
        canMakeNewGallery =
            (model.newGalleryInputEn /= Nothing)
                && (model.newGalleryInputFr /= Nothing)
    in
    column
        [ spacing 15
        , width fill
        ]
        [ row
            [ width fill
            , spacing 15
            ]
            [ column
                [ spacing 15 ]
                [ Input.text
                    textInputStyle
                    { onChange = NewGalleryEnTitle
                    , text =
                        model.newGalleryInputEn
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
                        model.newGalleryInputFr
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
                ]
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
        , column
            []
            (chunkedRows
                (min config.width 1000)
                (bestFit 200)
                (Dict.map (\k v -> ( k, v.titleImg |> Maybe.withDefault "" )) model.galleries
                    |> Dict.values
                    |> List.map (imgBlockView EditMeta)
                )
            )
        ]


galleryMetaView : ViewConfig -> Model msg -> String -> Element msg
galleryMetaView config model title =
    column
        [ spacing 15
        , width fill
        ]
        [ row
            [ centerX
            , spacing 15
            ]
            [ Input.button
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
        , case model.gallery of
            Just gallery ->
                column
                    [ padding 15
                    , Background.color white
                    , Border.color grey
                    , Border.rounded 5
                    , width (px (min config.width 1000))
                    , centerX
                    ]
                    [ Gallery.view config gallery ]

            _ ->
                Element.none
        ]


galleryImgView : ViewConfig -> Model msg -> String -> Element msg
galleryImgView config model title =
    case Dict.get title model.controllers of
        Just imgController ->
            ImageController.view config imgController

        _ ->
            Element.none


galleryArticleView : ViewConfig -> Model msg -> String -> Element msg
galleryArticleView config model title =
    MarkdownEditor.view config model.markdownEditor



-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------
