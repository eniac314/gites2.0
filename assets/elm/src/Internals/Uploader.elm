module Internals.Uploader exposing (..)

import Auth.AuthPlugin as Auth
import Base64
import Bytes.Encode
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import File exposing (..)
import Http exposing (..)
import Internals.Helpers exposing (Status(..), httpErrorToString)
import Json.Decode as D
import Json.Encode as E
import Murmur3 exposing (hashString)
import String.Extra exposing (leftOf, rightOf)
import Style.Helpers exposing (progressBar)


type alias Model msg =
    { toUpload : Maybe ToUpload
    , presignedUrl : Maybe String
    , progress : Maybe { sent : Int, size : Int }
    , uploadStatus : Status
    , error : Maybe String
    , outMsg : Msg -> msg
    }


init : (Msg -> msg) -> Model msg
init outMsg =
    { toUpload = Nothing
    , presignedUrl = Nothing
    , progress = Nothing
    , uploadStatus = Initial
    , error = Nothing
    , outMsg = outMsg
    }


subscriptions : Model msg -> Sub msg
subscriptions model =
    case model.presignedUrl of
        Just url ->
            Sub.map model.outMsg <|
                Http.track
                    (hashString 0 url
                        |> String.fromInt
                    )
                    GotProgress

        _ ->
            Sub.none


load : Model msg -> Auth.LogInfo -> ToUpload -> ( Model msg, Cmd msg )
load model logInfo toUpload =
    let
        fn =
            case toUpload of
                Base64Img { name } ->
                    name

                FileHandler f ->
                    File.name f
    in
    ( { model
        | toUpload = Just toUpload
        , presignedUrl = Nothing
      }
    , Cmd.map model.outMsg <|
        getPresignedUrl logInfo fn
    )


type ToUpload
    = Base64Img { name : String, data : String }
    | FileHandler File


type Msg
    = NoOp
    | Upload
    | PresignedUrl (Result Http.Error String)
    | GotProgress Http.Progress
    | UploadResult (Result Http.Error ())


update : { a | logInfo : Auth.LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        PresignedUrl res ->
            case res of
                Ok url ->
                    ( { model | presignedUrl = Just url }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Upload ->
            case ( model.toUpload, model.presignedUrl ) of
                ( Just (Base64Img { name, data }), Just url ) ->
                    ( { model | uploadStatus = Waiting }
                    , Cmd.map model.outMsg <|
                        uploadBase64Pic data url
                    )

                ( Just (FileHandler f), Just url ) ->
                    ( { model | uploadStatus = Waiting }
                    , Cmd.map model.outMsg <|
                        uploadFile f url
                    )

                _ ->
                    ( model, Cmd.none )

        GotProgress progress ->
            case progress of
                Sending ({ sent, size } as p) ->
                    ( { model | progress = Just p }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UploadResult r ->
            case r of
                Ok () ->
                    ( { model | uploadStatus = Success }
                    , Cmd.none
                    )

                Err httpError ->
                    ( { model
                        | uploadStatus = Failure
                        , error = Just (httpErrorToString httpError)
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )


getPresignedUrl logInfo filename =
    Auth.secureGet logInfo
        { url = "api/restricted/presigned_url/" ++ filename
        , expect =
            Http.expectJson PresignedUrl (D.field "presigned_s3_url" D.string)
        }


uploadBase64Pic dataUrl presignedUrl =
    let
        payload =
            rightOf "," dataUrl
                |> String.trim

        mime =
            leftOf ";" dataUrl
                |> rightOf ":"
                |> String.trim

        body =
            Base64.toBytes payload
                |> Maybe.withDefault
                    (Bytes.Encode.encode (Bytes.Encode.string ""))
                |> Http.bytesBody mime
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = presignedUrl
        , body = body
        , expect = Http.expectWhatever UploadResult
        , timeout = Nothing
        , tracker =
            hashString 0 presignedUrl
                |> String.fromInt
                |> Just
        }


uploadFile file presignedUrl =
    Http.request
        { method = "PUT"
        , headers = []
        , url = presignedUrl
        , body = Http.fileBody file
        , expect = Http.expectWhatever UploadResult
        , timeout = Nothing
        , tracker =
            hashString 0 presignedUrl
                |> String.fromInt
                |> Just
        }


view : Model msg -> Element msg
view model =
    Element.map model.outMsg <|
        column
            [ spacing 15
            ]
            [ paragraph
                []
                [ text (Maybe.withDefault "" model.presignedUrl) ]
            , case model.toUpload of
                Just (Base64Img { data }) ->
                    image
                        []
                        { src = data
                        , description = ""
                        }

                _ ->
                    Element.none
            , Input.button
                []
                { onPress = Just Upload
                , label = text "Upload"
                }
            ]
