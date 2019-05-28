module Backups.Backups exposing (BackupMeta, Model, Msg, init, load, update, view)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost, secureRequest)
import Bookings.DatePicker.Date exposing (formatDate)
import Date exposing (fromPosix)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import File exposing (..)
import File.Download as Download
import File.Select as Select
import Filesize exposing (format)
import Http exposing (..)
import Internals.Helpers exposing (..)
import Iso8601 exposing (..)
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (remove)
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
import Style.Palette exposing (..)
import Task exposing (..)
import Time exposing (..)


type alias Backup =
    { meta : BackupMeta
    , payload : String
    }


type alias BackupMeta =
    { key : String
    , size : Int
    , timestamp : Posix
    }


type alias Model msg =
    { loadingStatus : Status
    , backupMetas : Dict String BackupMeta
    , selectedBackupMetas : List String
    , uploadBuffer : Maybe Backup
    , outMsg : Msg -> msg
    }


type Msg
    = GotBackupMetas (Result Http.Error (Dict String BackupMeta))
    | SelectBackupMeta String
    | RequestBackupToExport String
    | GotBackupToExport String (Result Http.Error String)
    | DeleteBackups
    | BackupsDeleted (Result Http.Error ())
    | SelectBackupFile
    | GotBackupFile File
    | LoadBackupFileContents String
    | SendBackupFile
    | BackupSent (Result Http.Error ())
    | RequestManualBackup
    | ManualBackupDone (Result Http.Error ())
    | RestoreBackup String
    | BackupRestored (Result Http.Error ())
    | NoOp


init : (Msg -> msg) -> Model msg
init outMsg =
    { loadingStatus = Initial
    , backupMetas = Dict.empty
    , selectedBackupMetas = []
    , uploadBuffer = Nothing
    , outMsg = outMsg
    }


load : LogInfo -> Model msg -> ( Model msg, Cmd msg )
load logInfo model =
    ( { model
        | loadingStatus =
            case model.loadingStatus of
                Initial ->
                    Waiting

                _ ->
                    model.loadingStatus
      }
    , if model.loadingStatus == Initial then
        getBackupMetas logInfo model

      else
        Cmd.none
    )


update : { a | logInfo : LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        GotBackupMetas res ->
            case res of
                Ok metaDict ->
                    ( { model | backupMetas = metaDict }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SelectBackupMeta key ->
            case Dict.get key model.backupMetas of
                Just bm ->
                    ( { model
                        | selectedBackupMetas =
                            if List.member key model.selectedBackupMetas then
                                List.Extra.remove key model.selectedBackupMetas

                            else
                                key :: model.selectedBackupMetas
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RequestBackupToExport key ->
            ( model, requestBackupToExport config.logInfo model.outMsg key )

        GotBackupToExport filename res ->
            case res of
                Ok s ->
                    ( model
                    , Download.string filename "application/json" s
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteBackups ->
            let
                keys =
                    model.selectedBackupMetas
            in
            ( model, deleteBackups config.logInfo model.outMsg keys )

        BackupsDeleted res ->
            case res of
                Ok _ ->
                    ( { model | selectedBackupMetas = [] }
                    , getBackupMetas config.logInfo model
                    )

                _ ->
                    ( model, Cmd.none )

        SelectBackupFile ->
            ( model
            , Cmd.map model.outMsg <|
                Select.file [] GotBackupFile
            )

        GotBackupFile f ->
            ( model
            , Cmd.map model.outMsg <|
                Task.perform LoadBackupFileContents (File.toString f)
            )

        LoadBackupFileContents s ->
            ( { model
                | uploadBuffer =
                    D.decodeString decodeBackup s
                        |> Result.toMaybe
              }
            , Cmd.none
            )

        SendBackupFile ->
            case model.uploadBuffer of
                Just b ->
                    ( model
                    , sendBackupFile config.logInfo model.outMsg b
                    )

                Nothing ->
                    ( model, Cmd.none )

        BackupSent res ->
            case res of
                Ok _ ->
                    ( model, getBackupMetas config.logInfo model )

                _ ->
                    ( model, Cmd.none )

        RequestManualBackup ->
            ( model, requestManualBackup config.logInfo model.outMsg )

        ManualBackupDone res ->
            case res of
                Ok _ ->
                    ( model, getBackupMetas config.logInfo model )

                _ ->
                    ( model, Cmd.none )

        RestoreBackup key ->
            ( model, restoreBackup config.logInfo model.outMsg key )

        BackupRestored res ->
            ( model, getBackupMetas config.logInfo model )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


type alias ViewConfig a =
    { a | lang : Lang, zone : Time.Zone }


view : ViewConfig a -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ width fill
            , height fill
            , padding 15
            , spacing 15
            , Background.color lightGrey
            , Border.rounded 5
            ]
            [ el
                [ Font.bold ]
                (textM config.lang
                    (MultLangStr "Database backups:"
                        "Sauvegardes base de donnée:"
                    )
                )
            , controlsView config model
            , backupsView config model
            ]


controlsView : ViewConfig a -> Model msg -> Element Msg
controlsView config model =
    row
        [ spacing 15
        ]
        [ Input.button
            (buttonStyle True)
            { onPress = Just <| RequestManualBackup
            , label =
                textM config.lang
                    (MultLangStr "Manual backup"
                        "Sauvegarde manuelle"
                    )
            }
        , Input.button
            (buttonStyle True)
            { onPress = Just <| SelectBackupFile
            , label =
                Icons.file
                    (Icons.defOptions
                        |> Icons.color black
                        |> Icons.size 25
                    )
            }
        , Input.text
            textInputStyle
            { onChange = \_ -> NoOp
            , text =
                case model.uploadBuffer of
                    Just { meta } ->
                        meta.key

                    _ ->
                        ""
            , placeholder =
                Just <|
                    Input.placeholder []
                        (textM config.lang
                            (MultLangStr "no file selected"
                                "pas de fichier chargé"
                            )
                        )
            , label =
                Input.labelHidden ""
            }
        , Input.button
            (buttonStyle (model.uploadBuffer /= Nothing))
            { onPress =
                Maybe.map (\_ -> SendBackupFile) model.uploadBuffer
            , label =
                Icons.cloudUpload
                    (Icons.defOptions
                        |> Icons.color black
                        |> Icons.size 25
                    )
            }
        , Input.button
            (buttonStyle (model.selectedBackupMetas /= []))
            { onPress =
                if model.selectedBackupMetas /= [] then
                    Just DeleteBackups

                else
                    Nothing
            , label =
                row
                    [ spacing 10 ]
                    [ textM config.lang
                        (MultLangStr "Delete backups"
                            "Supprimer sauvegardes"
                        )
                    , Icons.trashcan
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                    ]
            }
        ]


backupsView : ViewConfig a -> Model msg -> Element Msg
backupsView config model =
    column
        [ spacing 10
        ]
        (List.map (backupView config model.selectedBackupMetas) (Dict.values model.backupMetas))


backupView : ViewConfig a -> List String -> BackupMeta -> Element Msg
backupView config selected { key, size, timestamp } =
    row
        [ spacing 15
        ]
        [ Input.checkbox
            [ centerY ]
            { onChange = \_ -> SelectBackupMeta key
            , icon = Input.defaultCheckbox
            , checked = List.member key selected
            , label = Input.labelHidden ""
            }
        , timestampView config timestamp size
        , Input.button
            (buttonStyle True)
            { onPress = Just <| RequestBackupToExport key
            , label =
                Icons.desktopDownload
                    (Icons.defOptions
                        |> Icons.color black
                        |> Icons.size 25
                    )
            }
        , Input.button
            (buttonStyle True)
            { onPress = Just <| RestoreBackup key
            , label =
                row
                    [ spacing 10 ]
                    [ textM config.lang
                        (MultLangStr "Restore backup"
                            "Restaurer sauvegarde"
                        )
                    , Icons.database
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                    ]
            }
        ]


timestampView : ViewConfig a -> Posix -> Int -> Element Msg
timestampView config timestamp size =
    row
        [ centerY
        , width (px 250)
        , spacing 15
        ]
        [ el
            []
            (text <|
                formatDate config.lang (Date.fromPosix config.zone timestamp)
                    ++ "  "
                    ++ (Time.toHour config.zone timestamp
                            |> String.fromInt
                            |> String.padLeft 2 '0'
                       )
                    ++ ":"
                    ++ (Time.toMinute config.zone timestamp
                            |> String.fromInt
                            |> String.padLeft 2 '0'
                       )
            )
        , text <| Filesize.format size
        ]



-------------------------------------------------------------------------------
-----------------------------------
-- Json handling and server coms --
-----------------------------------


decodeBackup : D.Decoder Backup
decodeBackup =
    D.map2 Backup
        (D.field "key"
            (D.string
                |> D.andThen
                    (\k ->
                        case Iso8601.toTime k of
                            Ok t ->
                                D.succeed <| BackupMeta k 0 t

                            Err _ ->
                                D.fail "can't decode timestamp"
                    )
            )
        )
        (D.field "payload" D.string)


decodeBackupMetas : D.Decoder (Dict String BackupMeta)
decodeBackupMetas =
    D.list
        (D.map2 Tuple.pair
            (D.field "key" D.string)
            (D.field "size"
                (D.string
                    |> D.map String.toInt
                    |> D.map (Maybe.withDefault 0)
                )
            )
            |> D.andThen
                (\( k, s ) ->
                    case Iso8601.toTime (formatKey k) of
                        Ok t ->
                            D.succeed <| BackupMeta k s t

                        Err _ ->
                            D.fail "can't decode timestamp"
                )
        )
        |> D.map (List.map (\bm -> ( bm.key, bm )))
        |> D.map Dict.fromList


formatKey k =
    String.dropLeft (String.length "Backups/") k |> String.replace " " "T"


getBackupMetas : LogInfo -> Model msg -> Cmd msg
getBackupMetas logInfo model =
    secureGet logInfo
        { url = "api/restricted/list_backups"
        , expect =
            Http.expectJson
                (model.outMsg << GotBackupMetas)
                decodeBackupMetas
        }


sendBackupFile : LogInfo -> (Msg -> msg) -> Backup -> Cmd msg
sendBackupFile logInfo outMsg backup =
    let
        body =
            E.object
                [ ( "key", E.string backup.meta.key )
                , ( "payload", E.string backup.payload )
                ]
                |> Http.jsonBody
    in
    securePost logInfo
        { url = "api/restricted/import_backup"
        , body = body
        , expect =
            Http.expectWhatever (outMsg << BackupSent)
        }


requestManualBackup : LogInfo -> (Msg -> msg) -> Cmd msg
requestManualBackup logInfo outMsg =
    secureGet logInfo
        { url = "api/restricted/manual_backup"
        , expect =
            Http.expectWhatever (outMsg << ManualBackupDone)
        }


requestBackupToExport : LogInfo -> (Msg -> msg) -> String -> Cmd msg
requestBackupToExport logInfo outMsg key =
    secureGet logInfo
        { url =
            "api/restricted/export_backup/"
                ++ String.dropLeft (String.length "Backups/") key
        , expect =
            Http.expectString (outMsg << GotBackupToExport key)
        }


deleteBackups : LogInfo -> (Msg -> msg) -> List String -> Cmd msg
deleteBackups logInfo outMsg keys =
    let
        body =
            E.object
                [ ( "keys", E.list E.string keys )
                ]
                |> Http.jsonBody
    in
    securePost logInfo
        { url = "api/restricted/delete_backups"
        , body = body
        , expect =
            Http.expectWhatever (outMsg << BackupsDeleted)
        }


restoreBackup : LogInfo -> (Msg -> msg) -> String -> Cmd msg
restoreBackup logInfo outMsg key =
    secureGet logInfo
        { url =
            "api/restricted/restore_backup/"
                ++ String.dropLeft (String.length "Backups/") key
        , expect =
            Http.expectWhatever (outMsg << BackupRestored)
        }
