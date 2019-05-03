module Internals.DocController exposing (..)

import Auth.AuthPlugin as Auth
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import File exposing (..)
import File.Select as Select exposing (..)
import Html as Html
import Html.Attributes as HtmlAttr
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.Uploader as Uploader
import Json.Decode as Decode
import Json.Encode as Encode
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
    , currentDoc : Maybe String
    , outMsg : Msg -> msg
    }


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { contents = Dict.empty
      , contentsLoadStatus = Initial
      , uploaders = Dict.empty
      , locked = Set.empty
      , currentLink = Nothing
      , outMsg = outMsg
      }
    , Cmd.none
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
    | SelectDoc String
    | DeleteDoc String
    | NoOp


update : { a | logInfo : Auth.LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config mgs model =
    case msg of
        FilesRequested ->
            ( model, Cmd.none )

        FilesSelected first remaining ->
            ( model, Cmd.none )

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
            ( model, Cmd.none )

        GotContents res ->
            ( model, Cmd.none )

        SelectDoc String ->
            ( model, Cmd.none )

        DeleteDoc String ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
-------------------
-- File handling --
-------------------


selectImages : Cmd Msg
selectImages =
    Select.files [ "application/pdf", "application/pdf" ] FilesSelected



-------------------------------------------------------------------------------
-------------------
-- Json Handling --
-------------------


getContents : Auth.LogInfo -> Model msg -> Cmd msg
getContents logInfo model =
    Auth.secureGet logInfo
        { url = "api/restricted/list_bucket/" ++ model.workingDirectory
        , expect =
            Http.expectJson
                (model.outMsg << GotContents)
                decodeContents
        }


decodeContents : Decode.Decoder (Dict String String)
decodeContents =
    Decode.field "content" (Decode.dict Decode.string)
