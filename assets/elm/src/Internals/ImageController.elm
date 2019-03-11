port module Internals.ImageController exposing (..)

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
import Element.Region as Region
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
import Style.Helpers exposing (..)
import Style.Palette exposing (..)
import Task exposing (perform)


port toImageProcessor : Encode.Value -> Cmd msg


port processedImages : (Decode.Value -> msg) -> Sub msg


processCmd model filename data =
    Cmd.map model.outMsg
        (toImageProcessor <|
            Encode.object
                [ ( "imageData", Encode.string data )
                , ( "filename", Encode.string filename )
                ]
        )


type alias Model msg =
    { workingDirectory : String
    , contents : Dict String ImageMeta
    , processingQueue : List ( String, File )
    , uploaders : Dict String (Uploader.Model msg)
    , sizes : Dict String { width : Int, height : Int }
    , picked : List ImageMeta
    , displayMode : DisplayMode
    , outMsg : Msg -> msg
    }


init : String -> (Msg -> msg) -> ( Model msg, Cmd msg )
init wc outMsg =
    ( { workingDirectory = wc
      , contents = Dict.empty
      , processingQueue = []
      , uploaders = Dict.empty
      , sizes = Dict.empty
      , picked = []
      , displayMode = DisplaySelector
      , outMsg = outMsg
      }
    , Cmd.batch
        []
        |> Cmd.map outMsg
    )


load : Model msg -> List ImageMeta -> Model msg
load model picked =
    { model | picked = picked }


subscriptions : Model msg -> Sub msg
subscriptions model =
    Sub.batch
        ([ processedImages (model.outMsg << ImageProcessed) ]
            ++ List.map
                (\uploader ->
                    Uploader.subscriptions uploader
                )
                (Dict.values model.uploaders)
        )


type DisplayMode
    = DisplaySelector
    | DisplayUploader


type Msg
    = ImagesRequested
    | ImagesSelected File (List File)
    | Base64Img String String
    | ImageProcessed Decode.Value
    | UploaderMsg String Uploader.Msg
    | GetContents
    | GotContents (Result Http.Error (Dict String ImageMeta))
    | GoBack
    | NoOp


update : { a | logInfo : Auth.LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg, Maybe (PluginResult (List ImageMeta)) )
update config msg model =
    case msg of
        ImagesRequested ->
            ( model
            , Cmd.map model.outMsg selectImages
            , Nothing
            )

        ImagesSelected first remaining ->
            let
                files =
                    first :: remaining
            in
            ( { model
                | processingQueue =
                    List.indexedMap
                        (\n f -> ( indexName (n + 1), f ))
                        remaining
              }
            , Task.perform
                (Base64Img (indexName 0))
                (File.toUrl first)
                |> Cmd.map model.outMsg
            , Nothing
            )

        Base64Img filename data ->
            ( model
            , processCmd model filename data
            , Nothing
            )

        ImageProcessed json ->
            case Decode.decodeValue decodeProcessedData json of
                Ok ({ content, filename } as pi) ->
                    let
                        ( cmd, processingQueue ) =
                            case model.processingQueue of
                                [] ->
                                    ( Cmd.none, [] )

                                ( filename_, file ) :: rest ->
                                    ( Task.perform
                                        (Base64Img filename_)
                                        (File.toUrl file)
                                        |> Cmd.map model.outMsg
                                    , rest
                                    )

                        outMsg_ s =
                            model.outMsg << UploaderMsg s

                        ( uploader, upCmd ) =
                            Uploader.load
                                (Uploader.init (outMsg_ filename) True)
                                config.logInfo
                                (Uploader.Base64Img
                                    { name =
                                        model.workingDirectory
                                            ++ "/"
                                            ++ filename
                                    , data = content
                                    , width = pi.width
                                    , height = pi.height
                                    }
                                )

                        ( thumbUploader, thUpCmd ) =
                            Uploader.load
                                (Uploader.init (outMsg_ <| "thumbs/" ++ filename) True)
                                config.logInfo
                                (Uploader.Base64Img
                                    { name =
                                        model.workingDirectory
                                            ++ "/thumbs/"
                                            ++ filename
                                    , data = pi.thumb
                                    , width = 200
                                    , height = 200
                                    }
                                )
                    in
                    ( { model
                        | processingQueue = processingQueue
                        , uploaders =
                            model.uploaders
                                |> Dict.insert filename uploader
                                |> Dict.insert ("thumbs/" ++ filename) thumbUploader
                        , sizes =
                            Dict.insert
                                filename
                                { width = pi.width
                                , height = pi.height
                                }
                                model.sizes
                      }
                    , Cmd.batch
                        [ cmd
                        , upCmd
                        , thUpCmd
                        ]
                    , Nothing
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        UploaderMsg filename uploaderMsg ->
            case Dict.get filename model.uploaders of
                Just uploader ->
                    let
                        ( newUploader, cmd ) =
                            Uploader.update config uploaderMsg uploader
                    in
                    ( { model
                        | uploaders =
                            Dict.insert filename newUploader model.uploaders
                      }
                    , cmd
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        GetContents ->
            ( model
            , getContents config.logInfo model
            , Nothing
            )

        GotContents res ->
            case res of
                Ok content ->
                    ( { model | contents = content }
                    , Cmd.none
                    , Nothing
                    )

                Err e ->
                    let
                        err =
                            Debug.log "" e
                    in
                    ( model, Cmd.none, Nothing )

        GoBack ->
            ( { model | picked = [] }
            , Cmd.none
            , Just PluginQuit
            )

        NoOp ->
            ( model, Cmd.none, Nothing )



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


view : { a | lang : Lang } -> Model msg -> Element msg
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

                --, Background.color red
                , width fill
                ]
                [ Input.button
                    (buttonStyle True)
                    { onPress = Just ImagesRequested
                    , label =
                        textM config.lang (MultLangStr "Upload images" "Mettre en ligne")
                    }
                , row
                    [ centerX
                    , spacing 10
                    ]
                    [ Input.button
                        (buttonStyle True)
                        { onPress = Just GoBack
                        , label =
                            textM config.lang (MultLangStr "Back" "Retour")
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress = Nothing
                        , label =
                            textM config.lang (MultLangStr "Save and quit" "Valider")
                        }
                    ]
                , if model.contents == Dict.empty then
                    Input.button
                        (buttonStyle True)
                        { onPress = Just GetContents
                        , label =
                            textM config.lang (MultLangStr "load pictures" "charger images")
                        }
                  else
                    Element.none
                ]
            ]



-------------------------------------------------------------------------------
-------------------
-- File handling --
-------------------


selectImages : Cmd Msg
selectImages =
    Select.files [ "image/png", "image/jpg" ] ImagesSelected



-------------------------------------------------------------------------------
-------------------
-- Json Handling --
-------------------


type alias ProcessedImage =
    { filename : String
    , content : String
    , thumb : String
    , size : Int
    , width : Int
    , height : Int
    }


decodeProcessedData : Decode.Decoder ProcessedImage
decodeProcessedData =
    Decode.map6 ProcessedImage
        (Decode.field "filename" Decode.string)
        (Decode.field "content" Decode.string)
        (Decode.field "thumb" Decode.string)
        (Decode.field "size" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


getContents : Auth.LogInfo -> Model msg -> Cmd msg
getContents logInfo model =
    Auth.secureGet logInfo
        { url = "api/restricted/list_bucket/" ++ model.workingDirectory
        , expect =
            Http.expectJson
                (model.outMsg << GotContents)
                decodeContents
        }


decodeContents : Decode.Decoder (Dict String ImageMeta)
decodeContents =
    let
        decodeSize =
            Decode.map2 (\w h -> { width = w, height = h })
                (Decode.field "width" Decode.int)
                (Decode.field "height" Decode.int)
    in
    Decode.field "content" (Decode.dict decodeSize)
        |> Decode.map
            (Dict.map
                (\url size ->
                    { url = url
                    , caption = Nothing
                    , size = size
                    }
                )
            )



-------------------------------------------------------------------------------
------------------
-- Misc Helpers --
------------------


indexName n =
    String.fromInt n
        |> String.padLeft 3 '0'
        |> strCons ".jpg"
