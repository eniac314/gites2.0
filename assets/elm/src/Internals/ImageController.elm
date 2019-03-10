port module Internals.ImageController exposing (..)

import Auth.AuthPlugin as Auth
import Dict exposing (..)
import File exposing (..)
import File.Select as Select exposing (..)
import Internals.Helpers exposing (..)
import Internals.Uploader as Uploader
import Json.Decode as Decode
import Json.Encode as Encode
import Style.Helpers exposing (..)
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


type DisplayMode
    = DisplaySelector
    | DisplayUploader


type Msg
    = ImagesRequested
    | ImagesSelected File (List File)
    | Base64Img String String
    | ImageProcessed Decode.Value
    | UploaderMsg String Uploader.Msg
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

        NoOp ->
            ( model, Cmd.none, Nothing )



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



-------------------------------------------------------------------------------
------------------
-- Misc Helpers --
------------------


indexName n =
    String.fromInt n
        |> String.padLeft 3 '0'
        |> strCons ".jpg"
