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
import Gallery.GalleryShared exposing (..)
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.ImageController as ImageController
import Internals.MarkdownEditor as MarkdownEditor
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)


type alias Model msg =
    { galleries : Dict String GalleryMeta
    , markdownEditor : MarkdownEditor.Model msg
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
    | EditImgs String
    | DeleteGallery String
    | GalleryDeleted (Result Http.Error ())
    | MarkdownEditorMsg MarkdownEditor.Msg
    | ImageControllerMsg String ImageController.Msg
    | GotGalleryMetas (Result Http.Error (Dict String GalleryMeta))
    | Save
    | Saved (Result Http.Error ())
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { galleries = Dict.empty
      , markdownEditor =
            MarkdownEditor.init
                French
                (outMsg << MarkdownEditorMsg)
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
        (Dict.map
            (\wd ic ->
                ImageController.subscriptions ic
            )
            model.controllers
            |> Dict.values
        )


update : { a | logInfo : LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
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
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        EditMeta title ->
            ( { model | displayMode = EditGalleryMeta title }
            , Cmd.none
            )

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

        NoOp ->
            ( model, Cmd.none )


view config model =
    Element.none



-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------
