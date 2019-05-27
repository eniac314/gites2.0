module Backups.Backups exposing (BackupMeta, Model)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost, secureRequest)
import Dict exposing (..)
import File exposing (..)
import File.Download as Download
import File.Select as Select
import Http exposing (..)
import Internals.Helpers exposing (..)
import Iso8601 exposing (..)
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (remove)
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
    , selectedBackupMetas : List BackupMeta
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
    ( { model | loadingStatus = Waiting }
    , getBackupMetas logInfo model
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
                            if List.member bm model.selectedBackupMetas then
                                List.Extra.remove bm model.selectedBackupMetas

                            else
                                bm :: model.selectedBackupMetas
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
                    List.map .key model.selectedBackupMetas
            in
            ( model, deleteBackups config.logInfo model.outMsg keys )

        BackupsDeleted res ->
            case res of
                Ok _ ->
                    ( model
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
            ( model, Cmd.none )

        BackupRestored res ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



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
            (D.field "size" D.int)
            |> D.andThen
                (\( k, s ) ->
                    case Iso8601.toTime k of
                        Ok t ->
                            D.succeed <| BackupMeta k s t

                        Err _ ->
                            D.fail "can't decode timestamp"
                )
        )
        |> D.map (List.map (\bm -> ( bm.key, bm )))
        |> D.map Dict.fromList


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
        { url = "api/restricted/export_backup/" ++ key
        , expect =
            Http.expectString (outMsg << GotBackupToExport key)
        }


deleteBackups : LogInfo -> (Msg -> msg) -> List String -> Cmd msg
deleteBackups logInfo outMsg keys =
    let
        body =
            E.list E.string keys
                |> Http.jsonBody
    in
    securePost logInfo
        { url = "api/restricted/delete_backups"
        , body = body
        , expect =
            Http.expectWhatever (outMsg << BackupsDeleted)
        }
