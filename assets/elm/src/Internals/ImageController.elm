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
import List.Extra exposing (remove, swapAt)
import MultLang.MultLang exposing (..)
import String.Extra exposing (leftOf, rightOfBack)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
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
    , mode : Mode
    , contents : Dict String ImageMeta
    , contentsLoadStatus : Status
    , portActive : Bool
    , processingQueue : List ( String, File )
    , uploaders : Dict String (Uploader.Model Msg)
    , sizes : Dict String { width : Int, height : Int }
    , picked : List ImageMeta
    , locked : List ImageMeta
    , output : Dict Int ImageMeta
    , displayMode : DisplayMode
    , outMsg : Msg -> msg
    }


init : String -> Mode -> (Msg -> msg) -> ( Model msg, Cmd msg )
init wc mode outMsg =
    ( { workingDirectory = wc
      , mode = mode
      , contents = Dict.empty
      , contentsLoadStatus = Initial
      , portActive = False
      , processingQueue = []
      , uploaders = Dict.empty
      , sizes = Dict.empty
      , picked = []
      , locked = []
      , output = Dict.empty
      , displayMode = DisplaySelector
      , outMsg = outMsg
      }
    , Cmd.batch
        []
        |> Cmd.map outMsg
    )


load : Auth.LogInfo -> Model msg -> List ImageMeta -> ( Model msg, Cmd msg )
load logInfo model output =
    ( { model
        | output =
            List.indexedMap Tuple.pair output
                |> Dict.fromList
        , contentsLoadStatus =
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
        ([ if model.portActive then
            processedImages (model.outMsg << ImageProcessed)
           else
            Sub.none
         ]
            ++ List.map
                (\uploader ->
                    Uploader.subscriptions uploader
                        |> Sub.map model.outMsg
                )
                (Dict.values model.uploaders)
        )


type Mode
    = RowMode
    | GalleryMode


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
    | PickImage ImageMeta
    | ConfirmSelection
    | DeletePicked
    | Deleted ImageMeta (Result Http.Error Bool)
    | SetCaption Int String
    | SwapUp Int
    | SwapDown Int
    | GoBack
    | SaveAndQuit
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
                jpgName f =
                    leftOf "." (File.name f) ++ ".jpg"

                files =
                    first :: remaining
            in
            ( { model
                | processingQueue =
                    List.map
                        (\f -> ( jpgName f, f ))
                        remaining
                , portActive = True
              }
            , Task.perform
                (Base64Img (jpgName first))
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
                        ( cmd, processingQueue, portActive ) =
                            case model.processingQueue of
                                [] ->
                                    ( Cmd.none, [], False )

                                ( filename_, file ) :: rest ->
                                    ( Task.perform
                                        (Base64Img filename_)
                                        (File.toUrl file)
                                        |> Cmd.map model.outMsg
                                    , rest
                                    , True
                                    )

                        outMsg_ s =
                            UploaderMsg s

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
                        , portActive = portActive
                      }
                    , Cmd.batch
                        [ cmd
                        , Cmd.map model.outMsg upCmd
                        , Cmd.map model.outMsg thUpCmd
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
                    ( { model
                        | contents = content
                        , contentsLoadStatus = Success
                      }
                    , Cmd.none
                    , Nothing
                    )

                Err e ->
                    --let
                    --    err =
                    --        Debug.log "" e
                    --in
                    ( { model | contentsLoadStatus = Failure }
                    , Cmd.none
                    , Nothing
                    )

        PickImage imgMeta ->
            ( { model
                | picked =
                    if List.member imgMeta model.picked then
                        List.Extra.remove imgMeta model.picked
                    else
                        model.picked ++ [ imgMeta ]
              }
            , Cmd.none
            , Nothing
            )

        ConfirmSelection ->
            ( { model
                | output =
                    List.indexedMap Tuple.pair model.picked
                        |> Dict.fromList
              }
            , Cmd.none
            , Nothing
            )

        DeletePicked ->
            ( { model
                | locked = model.picked
                , picked = []
              }
            , Cmd.batch
                (List.map (deleteImage config.logInfo model.outMsg) model.picked)
            , Nothing
            )

        Deleted img res ->
            case res of
                Ok True ->
                    ( { model
                        | contents = Dict.remove img.url model.contents
                        , locked = List.Extra.remove img model.picked
                      }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( { model
                        | locked = List.Extra.remove img model.picked
                      }
                    , Cmd.none
                    , Nothing
                    )

        SetCaption id s ->
            case Dict.get id model.output of
                Just img ->
                    ( { model
                        | output =
                            Dict.insert id
                                { img
                                    | caption =
                                        if s == "" then
                                            Nothing
                                        else
                                            Just (MultLangStr s s)
                                }
                                model.output
                      }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        SwapUp id ->
            let
                newOutput =
                    Dict.values model.output
                        |> swapAt id (id - 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            ( { model | output = newOutput }
            , Cmd.none
            , Nothing
            )

        SwapDown id ->
            let
                newOutput =
                    Dict.values model.output
                        |> swapAt id (id + 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            ( { model | output = newOutput }
            , Cmd.none
            , Nothing
            )

        GoBack ->
            ( { model | picked = [] }
            , Cmd.none
            , Just PluginQuit
            )

        SaveAndQuit ->
            ( { model | picked = [] }
            , Cmd.none
            , Just
                (PluginData
                    (Dict.values model.output)
                )
            )

        NoOp ->
            ( model, Cmd.none, Nothing )



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
                                Just ImagesRequested
                            else
                                Nothing
                        , label =
                            textM config.lang (MultLangStr "Upload images" "Mettre en ligne")
                        }
                    , Input.button
                        (buttonStyle (model.locked == []))
                        { onPress =
                            if model.locked == [] then
                                Just ConfirmSelection
                            else
                                Nothing
                        , label =
                            textM config.lang (MultLangStr "Confirm selection" "Valider selection")
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
                            textM config.lang (MultLangStr "reload pictures" "recharger images")
                        }
                  else
                    Element.none
                , mainPanelView config model
                , case model.mode of
                    RowMode ->
                        rowOutputView config model

                    GalleryMode ->
                        galleryOutPutView config model
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
                        { onPress = Just SaveAndQuit
                        , label =
                            textM config.lang (MultLangStr "Save and quit" "Valider")
                        }
                    ]
                ]
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
        , height (px 500)
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
                        (MultLangStr "Loading pictures..."
                            "Chargement des images en cours..."
                        )
                    )
                ]
            else
                imageSelectorView config model
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
                        (MultLangStr "Resizing and uploading pictures..."
                            "Compression et mise en ligne des images en cours..."
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


imageSelectorView : ViewConfig a -> Model msg -> List (Element Msg)
imageSelectorView config model =
    let
        imgBlockView ({ url } as imgMeta) =
            let
                isLocked =
                    List.member imgMeta model.locked
            in
            column
                ([ spacing 10
                 , padding 10
                 , width (px 150)
                 , alignTop
                 , if List.member imgMeta model.picked then
                    Background.color lightBlue
                   else
                    Background.color grey
                 , mouseOver
                    [ alpha 0.7 ]
                 , Border.rounded 5
                 ]
                    ++ (if isLocked then
                            [ alpha 0.5 ]
                        else
                            [ Events.onClick
                                (PickImage imgMeta)
                            , pointer
                            ]
                       )
                )
                [ el
                    [ width (px 140)
                    , height (px 105)
                    , centerX
                    , Background.image <|
                        awsUrl
                            ++ model.workingDirectory
                            ++ "/thumbs/"
                            ++ rightOfBack "/" url
                    ]
                    Element.none
                ]
    in
    chunkedRows (min config.width 1000)
        (bestFit 150)
        (Dict.values model.contents
            |> List.map imgBlockView
        )


rowOutputView config model =
    if model.output == Dict.empty then
        Element.none
    else
        column
            [ width (px <| min (config.width - 30) 1000)
            , centerX
            , spacing 15
            ]
            [ el
                [ centerX
                , Font.size 20
                , Font.family
                    [ Font.typeface "Montserrat"
                    , Font.sansSerif
                    ]
                ]
                (textM config.lang
                    (MultLangStr "Preview"
                        "Apercu"
                    )
                )
            , sameHeightImgRow
                awsUrl
                Nothing
                (Dict.values model.output)
            ]


galleryOutPutView config model =
    if model.output == Dict.empty then
        Element.none
    else
        column
            [ width (px <| min (config.width - 30) 1000)
            , centerX
            , spacing 15
            ]
            ([ el
                [ centerX
                , Font.size 20
                , Font.family
                    [ Font.typeface "Montserrat"
                    , Font.sansSerif
                    ]
                ]
                (textM config.lang
                    (MultLangStr "Preview"
                        "Apercu"
                    )
                )
             ]
                ++ (Dict.toList model.output
                        |> List.map (captionEditorView config model.workingDirectory)
                   )
            )


captionEditorView : ViewConfig a -> String -> ( Int, ImageMeta ) -> Element Msg
captionEditorView config wc ( id, { url, caption } ) =
    row
        [ width fill
        , Background.color grey
        , Border.rounded 5
        , padding 5
        , spacing 15
        ]
        [ el
            [ padding 5
            , Background.color lightGrey
            , Border.rounded 5
            ]
            (el
                [ width (px 140)
                , height (px 105)
                , centerX
                , Background.uncropped
                    (awsUrl
                        ++ wc
                        ++ "/thumbs/"
                        ++ rightOfBack "/" url
                    )
                , alignLeft
                ]
                Element.none
            )
        , Input.text
            (textInputStyle ++ [ alignLeft, width (px 400) ])
            { onChange = SetCaption id
            , text =
                Maybe.map (strM config.lang) caption
                    |> Maybe.withDefault ""
            , placeholder =
                Just <|
                    Input.placeholder []
                        (textM config.lang
                            (MultLangStr "Optional caption"
                                "Legende optionnelle"
                            )
                        )
            , label =
                Input.labelHidden ""
            }
        , column
            [ spacing 15
            , paddingXY 15 0
            , alignRight
            ]
            [ Input.button
                iconsStyle
                { onPress = Just <| SwapUp id
                , label =
                    Icons.arrowUp
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                }
            , Input.button
                iconsStyle
                { onPress = Just <| SwapDown id
                , label =
                    Icons.arrowDown
                        (Icons.defOptions
                            |> Icons.color black
                            |> Icons.size 25
                        )
                }
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
        decodeUnit =
            Decode.string
                |> Decode.map String.toInt
                |> Decode.map (Maybe.withDefault 0)

        decodeSize =
            Decode.map2 (\w h -> { width = w, height = h })
                (Decode.field "width" decodeUnit)
                (Decode.field "height" decodeUnit)
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


deleteImage : Auth.LogInfo -> (Msg -> msg) -> ImageMeta -> Cmd msg
deleteImage logInfo outMsg ({ url } as imgMeta) =
    Auth.secureGet logInfo
        { url = "api/restricted/delete_obj/" ++ String.replace "/" "¤" url
        , expect =
            Http.expectJson
                (outMsg << Deleted imgMeta)
                (Decode.field "message"
                    (Decode.map (\s -> s == "success") Decode.string)
                )
        }


deleteWorkingDirectory : Auth.LogInfo -> Model msg -> Cmd msg
deleteWorkingDirectory logInfo model =
    Auth.secureGet logInfo
        { url = "api/restricted/delete_folder/" ++ model.workingDirectory
        , expect =
            Http.expectWhatever
                (\_ -> model.outMsg NoOp)
        }



-------------------------------------------------------------------------------
------------------
-- Misc Helpers --
------------------


indexName n =
    String.fromInt n
        |> String.padLeft 3 '0'
        |> strCons ".jpg"
