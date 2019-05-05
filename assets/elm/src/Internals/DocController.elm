module Internals.DocController exposing (..)

import Auth.AuthPlugin as Auth
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import File exposing (..)
import File.Download as Download exposing (url)
import File.Select as Select exposing (..)
import Html as Html
import Html.Attributes as HtmlAttr
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.Uploader as Uploader
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (remove)
import MultLang.MultLang exposing (..)
import Set exposing (..)
import String.Extra exposing (leftOf, rightOfBack)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
import Style.Palette exposing (..)


type alias Model msg =
    { contents : Dict String String
    , contentsLoadStatus : Status
    , uploaders : Dict String (Uploader.Model Msg)
    , locked : Set String
    , picked : List String
    , outMsg : Msg -> msg
    }


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { contents = Dict.empty
      , contentsLoadStatus = Initial
      , uploaders = Dict.empty
      , locked = Set.empty
      , picked = []
      , outMsg = outMsg
      }
    , Cmd.none
    )


load : Auth.LogInfo -> Model msg -> ( Model msg, Cmd msg )
load logInfo model =
    ( { model
        | contentsLoadStatus =
            case model.contentsLoadStatus of
                Initial ->
                    Waiting

                _ ->
                    model.contentsLoadStatus
      }
    , if model.contentsLoadStatus == Initial then
        getContents logInfo model
      else
        Cmd.none
    )


subscriptions : Model msg -> Sub msg
subscriptions model =
    Sub.batch
        (List.map
            (\uploader ->
                Uploader.subscriptions uploader
                    |> Sub.map model.outMsg
            )
            (Dict.values model.uploaders)
        )


type Msg
    = FilesRequested
    | FilesSelected File (List File)
    | UploaderMsg String Uploader.Msg
    | GetContents
    | GotContents (Result Http.Error (Dict String String))
    | PickDoc String
    | DeletePicked
    | Deleted String (Result Http.Error ())
    | DownloadDoc String
    | NoOp


update : { a | logInfo : Auth.LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        FilesRequested ->
            ( model, Cmd.map model.outMsg selectFiles )

        FilesSelected first remaining ->
            let
                ( uploaders, cmds ) =
                    List.foldr
                        (\f ( acc, cmds_ ) ->
                            let
                                fn =
                                    File.name f

                                ( uploader, upCmd ) =
                                    Uploader.load
                                        (Uploader.init (UploaderMsg fn) True)
                                        config.logInfo
                                        (Uploader.FileHandler f)
                            in
                            ( ( fn, uploader ) :: acc, upCmd :: cmds_ )
                        )
                        ( [], [] )
                        (first :: remaining)
            in
            ( { model
                | uploaders =
                    Dict.fromList uploaders
              }
            , Cmd.batch cmds
                |> Cmd.map model.outMsg
            )

        UploaderMsg filename uploaderMsg ->
            case Dict.get filename model.uploaders of
                Just uploader ->
                    let
                        ( newUploader, cmd ) =
                            Uploader.update config uploaderMsg uploader

                        uploaders =
                            Dict.insert filename newUploader model.uploaders

                        ( refreshContentsCmd, contentsLoadStatus, uploaders_ ) =
                            if Dict.foldr (\_ u b -> Uploader.uploadDone u && b) True uploaders then
                                ( getContents config.logInfo model
                                , Waiting
                                , Dict.empty
                                )
                            else
                                ( Cmd.none, model.contentsLoadStatus, uploaders )
                    in
                    ( { model
                        | uploaders = uploaders_
                        , contentsLoadStatus = contentsLoadStatus
                      }
                    , Cmd.batch
                        [ Cmd.map model.outMsg cmd
                        , refreshContentsCmd
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GetContents ->
            ( model, getContents config.logInfo model )

        GotContents res ->
            case res of
                Ok contents ->
                    ( { model
                        | contents = contents
                        , contentsLoadStatus = Success
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model | contentsLoadStatus = Failure }, Cmd.none )

        PickDoc name ->
            ( { model
                | picked =
                    if List.member name model.picked then
                        List.Extra.remove name model.picked
                    else
                        model.picked ++ [ name ]
              }
            , Cmd.none
            )

        DeletePicked ->
            ( { model
                | locked = Set.fromList model.picked
                , picked = []
              }
            , Cmd.batch
                (List.map (deleteDoc config.logInfo model.outMsg) model.picked)
            )

        Deleted name res ->
            case res of
                Ok _ ->
                    ( { model
                        | contents = Dict.remove (String.replace "Documents/" "" name) model.contents
                        , locked = Set.remove name model.locked
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | locked = Set.remove name model.locked }
                    , Cmd.none
                    )

        DownloadDoc url ->
            ( model, Download.url url )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


type alias ViewConfig a =
    { a
        | lang : Lang
        , width : Int
    }


view : ViewConfig a -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ width fill
            , height fill
            , Background.color lightGrey
            ]
            [ column
                [ paddingXY 10 10
                , spacing 15
                , centerX
                , width fill
                ]
                [ row
                    [ spacing 15
                    , centerX
                    ]
                    [ Input.button
                        (buttonStyle <| model.contentsLoadStatus == Success)
                        { onPress =
                            if model.contentsLoadStatus == Success then
                                Just FilesRequested
                            else
                                Nothing
                        , label =
                            textM config.lang (MultLangStr "Upload files" "Mettre en ligne")
                        }
                    , Input.button
                        (buttonStyle (model.picked /= []))
                        { onPress =
                            if model.picked /= [] then
                                Just DeletePicked
                            else
                                Nothing
                        , label =
                            textM config.lang (MultLangStr "Delete selection" "Supprimer selection")
                        }
                    ]
                , if model.contentsLoadStatus == Failure then
                    Input.button
                        (buttonStyle True)
                        { onPress = Just GetContents
                        , label =
                            textM config.lang (MultLangStr "reload files" "recharger documents")
                        }
                  else
                    Element.none
                ]
            , mainPanelView config model
            ]


mainPanelView : ViewConfig a -> Model msg -> Element Msg
mainPanelView config model =
    column
        [ spacing 10
        , paddingXY 0 15
        , width (px <| min (config.width - 30) 1000)
        , Background.color (rgba 1 1 1 0.7)
        , Border.rounded 5
        , Border.width 1
        , Border.color black
        , height (px 700)
        , scrollbarY
        , centerX
        ]
        (if model.uploaders == Dict.empty then
            if model.contentsLoadStatus == Waiting then
                [ el
                    [ centerX
                    , centerY
                    ]
                    (textM config.lang
                        (MultLangStr "Loading files..."
                            "Chargement des fichiers en cours..."
                        )
                    )
                ]
            else
                [ fileSelectorView config model ]
         else
            [ column
                [ width fill
                , spacing 15
                , padding 15
                ]
                ([ el
                    [ centerX
                    ]
                    (textM config.lang
                        (MultLangStr "Uploading files..."
                            "Mise en ligne des fichiers en cours..."
                        )
                    )
                 ]
                    ++ (Dict.map
                            (\k u -> Uploader.view config u)
                            model.uploaders
                            |> Dict.values
                            |> List.reverse
                            |> List.intersperse
                                (el
                                    [ width fill
                                    , Border.color lightGrey
                                    , Border.width 1
                                    ]
                                    Element.none
                                )
                       )
                )
            ]
        )


fileSelectorView : ViewConfig a -> Model msg -> Element Msg
fileSelectorView config model =
    let
        fileBlockView url =
            let
                isLocked =
                    Set.member url model.locked

                filename =
                    String.replace "Documents/" "" url
                        |> String.replace ".pdf" ""
            in
            row
                [ spacing 10
                , padding 10
                , if List.member url model.picked then
                    Background.color lightBlue
                  else
                    Background.color grey
                , mouseOver
                    [ alpha 0.7 ]
                , Border.rounded 5
                , width fill
                ]
                [ row
                    (if isLocked then
                        [ alpha 0.5
                        , spacing 10
                        ]
                     else
                        [ Events.onClick
                            (PickDoc url)
                        , pointer
                        , width fill
                        , spacing 10
                        ]
                    )
                    [ el
                        [ width (px 35)
                        , height (px 35)
                        , Background.uncropped "/images/pdf.svg"
                        ]
                        Element.none
                    , el
                        []
                        (text filename)
                    ]
                , el
                    [ Events.onClick (DownloadDoc (awsUrl ++ url))
                    , alignRight
                    , pointer
                    ]
                    (Icons.linkExternal
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 20
                        )
                    )
                ]
    in
    column
        [ spacing 10
        , paddingXY 40 0
        , width fill
        ]
        (Dict.values model.contents
            |> List.map fileBlockView
        )


linkPicker : Model msg -> (String -> msg_) -> Element msg_
linkPicker model handler =
    let
        linkView filename url =
            el
                [ mouseOver
                    [ Font.color blue
                    ]
                , pointer
                , Events.onClick (handler url)
                ]
                (text filename)
    in
    column
        []
        (Dict.map linkView model.contents
            |> Dict.values
        )



-------------------------------------------------------------------------------
-------------------
-- File handling --
-------------------


selectFiles : Cmd Msg
selectFiles =
    Select.files [ "application/pdf", "application/pdf" ] FilesSelected



-------------------------------------------------------------------------------
-------------------
-- Json Handling --
-------------------


getContents : Auth.LogInfo -> Model msg -> Cmd msg
getContents logInfo model =
    Auth.secureGet logInfo
        { url = "api/restricted/list_bucket/Documents"
        , expect =
            Http.expectJson
                (model.outMsg << GotContents)
                decodeContents
        }


decodeContents : Decode.Decoder (Dict String String)
decodeContents =
    Decode.field "content"
        (Decode.keyValuePairs (Decode.succeed ""))
        |> Decode.map
            (List.map
                (\( k, _ ) -> ( String.replace "Documents/" "" k, k ))
            )
        |> Decode.map Dict.fromList


deleteDoc : Auth.LogInfo -> (Msg -> msg) -> String -> Cmd msg
deleteDoc logInfo outMsg url =
    Auth.secureGet logInfo
        { url = "api/restricted/delete_obj/" ++ String.replace "/" "¤" url
        , expect =
            Http.expectWhatever
                (outMsg << Deleted url)
        }
