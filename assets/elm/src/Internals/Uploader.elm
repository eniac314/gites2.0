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
import MultLang.MultLang exposing (..)
import Murmur3 exposing (hashString)
import String.Extra exposing (leftOf, rightOf)
import Style.Helpers exposing (buttonStyle_, okMark, progressBar)
import Style.Palette exposing (..)


type alias Model msg =
    { toUpload : Maybe ToUpload
    , presignedUrl : Maybe String
    , progress : Maybe { sent : Int, size : Int }
    , uploadStatus : Status
    , error : Maybe String
    , uploadAuto : Bool
    , outMsg : Msg -> msg
    }


init : (Msg -> msg) -> Bool -> Model msg
init outMsg uploadAuto =
    { toUpload = Nothing
    , presignedUrl = Nothing
    , progress = Nothing
    , uploadStatus = Initial
    , error = Nothing
    , outMsg = outMsg
    , uploadAuto = uploadAuto
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
            filename toUpload

        ( mime, meta ) =
            case toUpload of
                Base64Img { data, width, height } ->
                    ( leftOf ";" data
                        |> rightOf ":"
                        |> String.trim
                    , [ ( "width", String.fromInt width )
                      , ( "height", String.fromInt height )
                      ]
                    )

                FileHandler f ->
                    ( File.mime f, [] )
    in
    ( { model
        | toUpload = Just toUpload
        , presignedUrl = Nothing
      }
    , Cmd.map model.outMsg <|
        getPresignedUrl logInfo meta mime fn
    )


uploadDone model =
    model.uploadStatus == Success


type ToUpload
    = Base64Img
        { name : String
        , data : String
        , width : Int
        , height : Int
        }
    | FileHandler File


filename : ToUpload -> String
filename toUpload =
    case toUpload of
        Base64Img { name } ->
            name

        FileHandler f ->
            "Documents/" ++ File.name f


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
                    if model.uploadAuto then
                        upload { model | presignedUrl = Just url }
                    else
                        ( { model | presignedUrl = Just url }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        Upload ->
            upload model

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


upload : Model msg -> ( Model msg, Cmd msg )
upload model =
    case ( model.toUpload, model.presignedUrl ) of
        ( Just (Base64Img { name, data, width, height }), Just url ) ->
            ( { model | uploadStatus = Waiting }
            , Cmd.map model.outMsg <|
                uploadBase64Pic width height data url
            )

        ( Just (FileHandler f), Just url ) ->
            ( { model | uploadStatus = Waiting }
            , Cmd.map model.outMsg <|
                uploadFile f url
            )

        _ ->
            ( model, Cmd.none )


getPresignedUrl : Auth.LogInfo -> List ( String, String ) -> String -> String -> Cmd Msg
getPresignedUrl logInfo metadata mime fn =
    let
        body =
            E.object
                [ ( "mime"
                  , E.string mime
                  )
                , ( "filename"
                  , E.string (String.replace "/" "¤" fn)
                  )
                , ( "metadata"
                  , E.object
                        (List.map (\( k, v ) -> ( k, E.string v )) metadata)
                  )
                ]
                |> Http.jsonBody
    in
    Auth.securePost logInfo
        { url = "api/restricted/presigned_url/"
        , body = body
        , expect =
            Http.expectJson PresignedUrl (D.field "presigned_s3_url" D.string)
        }


uploadBase64Pic width height dataUrl presignedUrl =
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


view : { a | lang : Lang } -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ spacing 15
            , height (px 55)
            ]
            [ row
                []
                [ textM config.lang
                    (MultLangStr "Uploading: "
                        "Mise en ligne: "
                    )
                , el
                    [ Font.family
                        [ Font.monospace ]
                    , Font.color lightCharcoal
                    ]
                    (Maybe.map filename model.toUpload
                        |> Maybe.withDefault ""
                        |> text
                    )
                ]
            , case model.uploadStatus of
                Initial ->
                    progressBar 0

                Waiting ->
                    model.progress
                        |> Maybe.map
                            (\{ sent, size } ->
                                (toFloat sent / toFloat size) * 100
                            )
                        |> Maybe.map floor
                        |> Maybe.withDefault 0
                        |> progressBar

                Success ->
                    row
                        [ spacing 5 ]
                        [ el
                            []
                            (textM config.lang
                                (MultLangStr
                                    "Success"
                                    "Succes"
                                )
                            )
                        , okMark
                        ]

                Failure ->
                    column
                        [ spacing 15 ]
                        [ row
                            [ spacing 5 ]
                            [ el
                                []
                                (textM config.lang
                                    (MultLangStr
                                        "Failure: "
                                        "Echec: "
                                    )
                                )
                            , model.error
                                |> Maybe.withDefault ""
                                |> text
                            ]
                        , Input.button
                            (buttonStyle_ True)
                            { onPress =
                                Just Upload
                            , label =
                                textM config.lang
                                    (MultLangStr "Try again"
                                        "Réessayer"
                                    )
                            }
                        ]
            ]
