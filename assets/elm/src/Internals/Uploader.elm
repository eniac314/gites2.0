module Internals.Uploader exposing (..)

import Auth.AuthPlugin exposing (..)
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
import Json.Decode as D
import Json.Encode as E


type alias Model msg =
    { toUpload : Maybe ToUpload
    , presignedUrl : Maybe String
    , outMsg : Msg -> msg
    }


init : (Msg -> msg) -> Model msg
init outMsg =
    { toUpload = Nothing
    , presignedUrl = Nothing
    , outMsg = outMsg
    }


load : Model msg -> ToUpload -> ( Model msg, Cmd msg )
load model toUpload =
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
        getPresignedUrl fn
    )


type ToUpload
    = Base64Img { name : String, data : String }
    | FileHandler File


type Msg
    = NoOp
    | Upload
    | PresignedUrl (Result Http.Error String)


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
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
                    ( model
                    , Cmd.map model.outMsg <|
                        uploadBase64Pic data url
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getPresignedUrl filename =
    Http.get
        { url = "api/presigned_url/" ++ filename
        , expect =
            Http.expectJson PresignedUrl (D.field "presigned_url" D.string)
        }


uploadBase64Pic data url =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.stringBody "image/jpeg" data
        , expect = Http.expectWhatever (\_ -> NoOp)
        , timeout = Nothing
        , tracker = Nothing
        }


view : Model msg -> Element msg
view model =
    Element.map model.outMsg <|
        column
            [ spacing 15 ]
            [ text (Maybe.withDefault "" model.presignedUrl)
            , Input.button
                []
                { onPress = Just Upload
                , label = text "Upload"
                }
            ]
