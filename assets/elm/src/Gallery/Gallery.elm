module Gallery.Gallery exposing (..)

import Animation
    exposing
        ( Animation
        , animate
        , animation
        , duration
        , ease
        , from
        , getDuration
        , isDone
        , retarget
        , speed
        , to
        )
import Browser.Events exposing (Visibility(..), onAnimationFrame, onKeyDown, onVisibilityChange)
import Dict exposing (..)
import Ease exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Element.Region as Region
import Gallery.GalleryShared exposing (..)
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Internals.Helpers exposing (..)
import Internals.Streams as Streams exposing (..)
import Json.Decode as Decode
import MultLang.MultLang exposing (..)
import Set exposing (..)
import String.Extra exposing (rightOfBack, toSentenceCase)
import Style.Helpers exposing (..)
import Style.Icons exposing (..)
import Style.Palette exposing (..)
import Task exposing (..)
import Time exposing (Posix, every, millisToPosix, now, posixToMillis)


type alias Model msg =
    { loaded : Set String
    , loadedThumbs : Set String
    , key : String
    , ordering : Int
    , title : MultLangStr
    , titleImg : Maybe String
    , header : Maybe MultLangStr
    , article : Maybe MultLangStr
    , imagesStream : BiStream (List ImageMeta)
    , imagesSrcs : List String
    , mbDrag : Maybe Drag
    , mbAnim : Maybe ( Animation, Direction )
    , clock : Float
    , tickSubOn : Bool
    , displayMode : DisplayMode
    , outMsg : Msg -> msg
    }


type DisplayMode
    = DisplayImage
    | DisplayGrid


type Msg
    = ToogleDisplayMode
    | Select String
    | Next
    | Previous
    | Animate Direction Posix
    | DragStart Position
    | DragAt Position
    | DragEnd
    | ImgLoaded String
    | ThumbLoaded String
    | Tick Posix
    | NoOp


subscriptions model =
    Sub.map model.outMsg <|
        Sub.batch
            [ if model.tickSubOn then
                onAnimationFrame Tick
              else if model.mbAnim == Nothing && model.mbDrag == Nothing then
                Sub.none
              else
                onAnimationFrame Tick
            ]


init : GalleryMeta -> (Msg -> msg) -> Model msg
init { key, ordering, title, titleImg, article, header, album } outMsg =
    let
        images =
            case List.reverse album of
                h :: t ->
                    List.reverse <| t ++ [ h ]

                _ ->
                    album

        stream =
            images
                |> (\xs -> biStream xs dummyPic)
                |> chunkBiStream 3
    in
    { loaded = Set.empty
    , loadedThumbs = Set.empty
    , imagesStream = stream
    , imagesSrcs = List.map .url album
    , mbDrag = Nothing
    , mbAnim = Nothing
    , clock = 0
    , key = key
    , ordering = ordering
    , title = title
    , titleImg = titleImg
    , header = header
    , article = article
    , tickSubOn = False
    , displayMode = DisplayImage
    , outMsg = outMsg
    }



-------------------------------------------------------------------------------
-------------
-- Update ---
-------------


type alias Config a =
    { a
        | width : Int
    }


maxWidth config =
    min 650 <|
        min config.width
            960


update : Config a -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        ToogleDisplayMode ->
            case model.displayMode of
                DisplayImage ->
                    ( { model | displayMode = DisplayGrid }
                    , Cmd.none
                    )

                DisplayGrid ->
                    ( { model | displayMode = DisplayImage }
                    , Cmd.none
                    )

        Select url ->
            ( { model
                | imagesStream =
                    Streams.goTo model.imagesStream
                        (\chunk ->
                            case chunk of
                                l :: c :: r :: [] ->
                                    c.url == url

                                _ ->
                                    False
                        )
                , displayMode = DisplayImage
              }
            , Cmd.none
            )

        Next ->
            case model.mbAnim of
                Just _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | mbAnim = Nothing
                        , tickSubOn = True
                      }
                    , Cmd.map model.outMsg <|
                        Task.perform (Animate AnimateLeft) Time.now
                    )

        Previous ->
            case model.mbAnim of
                Just _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | mbAnim = Nothing
                        , tickSubOn = True
                      }
                    , Cmd.map model.outMsg <|
                        Task.perform (Animate AnimateRight) Time.now
                    )

        Animate dir t ->
            let
                newClock =
                    toFloat <| posixToMillis t

                newAnim =
                    case model.mbAnim of
                        Nothing ->
                            Just
                                ( animation newClock
                                    |> from 0
                                    |> to (toFloat (maxWidth config))
                                    |> speed 1
                                    |> ease Ease.inOutCirc
                                , dir
                                )

                        _ ->
                            Nothing
            in
            ( { model
                | mbAnim = newAnim
              }
            , Cmd.none
            )

        DragStart position ->
            case model.mbAnim of
                Nothing ->
                    ( { model
                        | mbDrag = Just (Drag position position)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DragAt position ->
            let
                updateDrag current_ (Drag start _) =
                    Drag start current_
            in
            case model.mbAnim of
                Nothing ->
                    ( { model
                        | mbDrag =
                            Maybe.map (updateDrag position) model.mbDrag
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DragEnd ->
            case ( model.mbAnim, model.mbDrag ) of
                ( Nothing, Just (Drag start current_) ) ->
                    let
                        newAnimFun x =
                            animation model.clock
                                |> from (toFloat x)
                                |> to (toFloat (maxWidth config))
                                |> speed 1
                                |> ease Ease.inOutCirc
                    in
                    if (start.x - current_.x) > 10 then
                        let
                            newAnim =
                                case model.mbAnim of
                                    Nothing ->
                                        Just
                                            ( newAnimFun (start.x - current_.x)
                                            , AnimateLeft
                                            )

                                    _ ->
                                        Nothing
                        in
                        ( { model
                            | mbAnim = newAnim
                            , mbDrag = Nothing
                          }
                        , Cmd.none
                        )
                    else if start.x - current_.x < -10 then
                        let
                            newAnim =
                                case model.mbAnim of
                                    Nothing ->
                                        Just
                                            ( newAnimFun (abs (start.x - current_.x))
                                            , AnimateRight
                                            )

                                    _ ->
                                        Nothing
                        in
                        ( { model
                            | mbAnim = newAnim
                            , mbDrag = Nothing
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | mbDrag = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick t ->
            let
                newClock =
                    toFloat <| posixToMillis t

                ( newAnim, newImages, tickSubOn ) =
                    case model.mbAnim of
                        Just ( anim, AnimateLeft ) ->
                            if isDone newClock anim then
                                ( Nothing, right (.imagesStream model), False )
                            else
                                ( model.mbAnim, model.imagesStream, True )

                        Just ( anim, AnimateRight ) ->
                            if isDone newClock anim then
                                ( Nothing, left (.imagesStream model), False )
                            else
                                ( model.mbAnim, model.imagesStream, True )

                        _ ->
                            ( model.mbAnim, model.imagesStream, model.tickSubOn )
            in
            ( { model
                | clock = newClock
                , mbAnim = newAnim
                , imagesStream = newImages
                , tickSubOn = tickSubOn
              }
            , Cmd.none
            )

        ImgLoaded url ->
            ( { model | loaded = Set.insert url model.loaded }
            , Cmd.none
            )

        ThumbLoaded url ->
            ( { model | loadedThumbs = Set.insert url model.loadedThumbs }
            , Cmd.none
            )

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
    let
        w =
            maxWidth config

        h =
            min 600 (round <| toFloat w / 1.333333)
    in
    Element.map model.outMsg <|
        column
            [ Background.color lightCharcoal
            , centerX
            ]
            [ titleRow config model
            , el
                [ Background.color white ]
                (case model.displayMode of
                    DisplayImage ->
                        galleryView config model

                    DisplayGrid ->
                        gridView config model
                )
            , captionRow config model
            ]


titleRow : ViewConfig a -> Model msg -> Element Msg
titleRow config model =
    let
        w =
            maxWidth config
    in
    row
        [ width (maximum w fill)
        , paddingXY 15 5
        , spacing 20
        ]
        [ el
            [ Font.size 30
            , Font.center
            , Font.color white
            , Font.family
                [ Font.typeface "Great Vibes"
                , Font.serif
                ]
            ]
            (text <|
                String.Extra.toSentenceCase
                    (strM config.lang model.title)
            )
        , Input.button
            (buttonStyle True
                ++ [ alignRight
                   ]
            )
            { onPress = Just ToogleDisplayMode
            , label =
                el [ paddingXY 0 0 ]
                    (html <|
                        case model.displayMode of
                            DisplayImage ->
                                grid 22

                            DisplayGrid ->
                                imageIcon 22
                    )
            }
        ]


captionRow : ViewConfig a -> Model msg -> Element Msg
captionRow config model =
    let
        w =
            maxWidth config
    in
    case current model.imagesStream of
        l :: c :: r :: [] ->
            row
                [ width (maximum w fill)
                , padding 15
                , spacing 20
                , height (minimum 20 fill)
                , Font.color white
                ]
                [ paragraph
                    []
                    [ text <|
                        (Maybe.map (strM config.lang) c.caption
                            |> Maybe.withDefault ""
                        )
                    ]
                , el
                    [ alignRight ]
                    (text
                        ((String.fromInt (1 + model.imagesStream.index)
                            |> String.padLeft 2 '0'
                         )
                            ++ "/"
                            ++ (String.fromInt model.imagesStream.size
                                    |> String.padLeft 2 '0'
                               )
                        )
                    )
                ]

        _ ->
            Element.none


galleryView : ViewConfig a -> Model msg -> Element Msg
galleryView config model =
    let
        w =
            maxWidth config

        h =
            min 600 (round <| toFloat w / 1.333333)

        controlPanel attr msg icon =
            el
                ([ if config.width > 750 then
                    width (px (w // 3))
                   else
                    width (px 50)
                 , height (px h)
                 , pointer
                 , alpha 0
                 , mouseOver
                    [ alpha 1 ]
                 , Events.onClick msg
                 , paddingXY 5 0
                 ]
                    ++ attr
                )
                (el
                    ([ Font.bold
                     , centerY
                     , Font.color lightCharcoal
                     , Border.rounded 20
                     , Background.color (rgba 255 255 255 0.7)
                     ]
                        ++ attr
                    )
                    (html <|
                        icon
                            (if config.width > 750 then
                                55
                             else
                                40
                            )
                    )
                )
    in
    el
        [ centerX
        , clipX
        , width (px w)
        , inFront <|
            controlPanel [] Previous chevronLeft_
        , inFront <|
            controlPanel [ alignRight ] Next chevronRight_
        ]
        (chunkView config model (current model.imagesStream))


chunkView : ViewConfig a -> Model msg -> List ImageMeta -> Element Msg
chunkView config model chunk =
    let
        w =
            maxWidth config

        h =
            min 600 (round <| toFloat w / 1.333333)
    in
    case chunk of
        l :: c :: r :: [] ->
            Lazy.lazy
                (\mc ->
                    row
                        (events model.mbDrag ( DragStart, DragAt, DragEnd )
                            ++ [ mc ]
                        )
                        [ picView config model l
                        , picView config model c
                        , picView config model r
                        ]
                )
                (moveChunk config model)

        _ ->
            Element.none


moveChunk : ViewConfig a -> Model msg -> Attribute Msg
moveChunk config model =
    let
        w =
            maxWidth config

        animFun =
            case model.mbAnim of
                Nothing ->
                    -- necessary in order to center the row
                    moveLeft (toFloat w)

                Just ( anim, AnimateLeft ) ->
                    moveLeft (toFloat w + animate model.clock anim)

                Just ( anim, AnimateRight ) ->
                    moveRight ((toFloat <| -1 * w) + animate model.clock anim)
    in
    case model.mbDrag of
        Nothing ->
            animFun

        Just (Drag start stop) ->
            if start.x - stop.x <= 0 then
                moveRight <| (toFloat <| (-1 * w) + abs (start.x - stop.x))
            else
                moveLeft (toFloat <| w + start.x - stop.x)


picView : ViewConfig a -> Model msg -> ImageMeta -> Element Msg
picView config model { url, size } =
    let
        w =
            maxWidth config

        h =
            min 600 (round <| toFloat w / 1.333333)
    in
    if Set.member url model.loaded then
        el
            [ Background.tiled "/images/canvas.png"
            , behindContent
                (el
                    [ Background.color white
                    , alpha 0.5
                    , width (px w)
                    , height (px h)
                    ]
                    Element.none
                )
            ]
            (el
                ([ width (px w)
                 , height (px h)
                 , Background.uncropped (awsUrl ++ url)
                 ]
                    ++ unselectable
                )
                Element.none
            )
    else
        column
            [ Background.tiled "/images/canvas.png"
            , width (px w)
            , height (px h)
            ]
            [ image
                [ centerX
                , centerY
                , clip
                ]
                { src = "/images/loading.gif"
                , description = "chargement en cours"
                }
            , html <|
                Html.img
                    [ HtmlAttr.hidden True
                    , HtmlEvents.on "load" (Decode.succeed (ImgLoaded url))
                    , HtmlAttr.src (awsUrl ++ url)
                    ]
                    []
            ]


gridView : ViewConfig a -> Model msg -> Element Msg
gridView config model =
    let
        w =
            maxWidth config

        h =
            min 600 (round <| toFloat w / 1.333333)

        thumbSize =
            135

        chunkSize =
            (w - 15) // (thumbSize + 10)

        rows =
            chunk chunkSize
                (List.map thumbView model.imagesSrcs)
                |> List.map
                    (row
                        [ width fill
                        , spaceEvenly
                        , paddingXY 10 0
                        ]
                    )

        thumbView url =
            if Set.member url model.loadedThumbs then
                el
                    [ padding 5
                    , Background.color lightCharcoal
                    , mouseOver
                        [ Background.color darkGrey ]
                    , Border.rounded 5
                    , Events.onClick (Select url)
                    , pointer
                    ]
                    (el
                        [ width (px thumbSize)
                        , height (px thumbSize)
                        , centerX
                        , mouseOver
                            [ alpha 0.5 ]
                        , Background.uncropped (awsUrl ++ thumbSrc url)
                        ]
                        Element.none
                    )
            else
                el
                    [ Background.color lightCharcoal
                    , Border.rounded 5
                    ]
                    (column
                        [ padding 5
                        , Border.rounded 5
                        , pointer
                        , width (px <| thumbSize + 10)
                        , height (px <| thumbSize + 10)
                        , Background.image "/images/loadingSmall.gif"
                        ]
                        [ html <|
                            Html.img
                                [ HtmlAttr.hidden True
                                , HtmlEvents.on "load" (Decode.succeed (ThumbLoaded url))
                                , HtmlAttr.src (awsUrl ++ thumbSrc url)
                                ]
                                []
                        ]
                    )
    in
    column
        [ Background.tiled "/images/canvas.png"
        , width (px w)
        , height (px h)
        , scrollbarY
        , spacing 10
        , paddingXY 0 10
        ]
        rows



--[ wrappedRow [ padding 10 ]
--    (List.map thumbView model.imagesSrcs)
--]
-------------------------------------------------------------------------------
------------------
-- Misc Helpers --
------------------


extractFileName img =
    rightOfBack "/" img.url
