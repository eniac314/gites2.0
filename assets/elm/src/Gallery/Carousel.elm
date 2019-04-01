module Gallery.Carousel exposing (..)

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
import Internals.Streams exposing (..)
import Json.Decode as Decode
import Set exposing (..)
import Style.Helpers exposing (..)
import Style.Icons exposing (..)
import Style.Palette exposing (..)
import Time exposing (Posix, every, millisToPosix, posixToMillis)


type alias Model msg =
    { loaded : Set String
    , images : BiStream (List Image)
    , mbDrag : Maybe Drag
    , mbAnim : Maybe ( Animation, AnimType )
    , clock : Float
    , visibility : Visibility
    , externalMsg : Msg -> msg
    }


type alias Image =
    { src : String
    , id : Int
    }


type AnimType
    = AnimateLeft
    | AnimateRight
    | AlphaFade


type Msg
    = Animate AnimType Posix
    | DragStart Position
    | DragAt Position
    | DragEnd
    | Tick Posix
    | ImgLoaded String
    | VisibilityChange Visibility
    | NoOp


subscriptions model =
    Sub.map model.externalMsg <|
        Sub.batch
            [ if model.mbAnim == Nothing && model.mbDrag == Nothing then
                Sub.none
              else
                onAnimationFrame Tick
            , if
                (model.visibility
                    == Hidden
                )
                    || (model.mbAnim /= Nothing)
                    || (model.mbDrag /= Nothing)
              then
                Sub.none
              else
                Time.every 10000 (Animate AlphaFade)
            , onVisibilityChange VisibilityChange
            ]


init : List String -> (Msg -> msg) -> Model msg
init imgs externalMsg =
    let
        stream =
            List.indexedMap
                (\n s -> Image s n)
                imgs
                |> (\xs -> biStream xs (Image "" -1))
                |> chunkBiStream 3
    in
    { loaded = Set.empty
    , images = stream
    , mbDrag = Nothing
    , mbAnim = Nothing
    , clock = 0
    , visibility = Visible
    , externalMsg = externalMsg
    }


update : { maxWidth : Int } -> Msg -> Model msg -> Model msg
update config msg model =
    let
        maxWidth =
            min (config.maxWidth - 30)
                970
    in
    case msg of
        Animate dir t ->
            let
                newClock =
                    toFloat <| posixToMillis t

                newAnim =
                    case ( model.mbAnim, dir ) of
                        ( Nothing, AnimateLeft ) ->
                            Just
                                ( animation newClock
                                    |> from 0
                                    |> to (toFloat maxWidth)
                                    |> speed 1
                                    |> ease Ease.inOutExpo
                                , AnimateLeft
                                )

                        ( Nothing, AlphaFade ) ->
                            Just
                                ( animation newClock
                                    |> from 1
                                    |> to 0
                                    |> duration 1500
                                    |> ease Ease.linear
                                , AlphaFade
                                )

                        _ ->
                            Nothing
            in
            { model | mbAnim = newAnim }

        DragStart position ->
            case model.mbAnim of
                Nothing ->
                    { model
                        | mbDrag = Just (Drag position position)
                    }

                _ ->
                    model

        DragAt position ->
            let
                updateDrag current_ (Drag start _) =
                    Drag start current_
            in
            case model.mbAnim of
                Nothing ->
                    { model
                        | mbDrag =
                            Maybe.map (updateDrag position) model.mbDrag
                    }

                _ ->
                    model

        DragEnd ->
            case ( model.mbAnim, model.mbDrag ) of
                ( Nothing, Just (Drag start current_) ) ->
                    let
                        newAnimFun x =
                            animation model.clock
                                |> from (toFloat x)
                                |> to (toFloat maxWidth)
                                |> speed 1
                                |> ease Ease.inOutExpo
                    in
                    if start.x - current_.x > 10 then
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
                        { model
                            | mbAnim = newAnim
                            , mbDrag = Nothing
                        }
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
                        { model
                            | mbAnim = newAnim
                            , mbDrag = Nothing
                        }
                    else
                        { model | mbDrag = Nothing }

                _ ->
                    model

        ImgLoaded src ->
            { model | loaded = Set.insert src model.loaded }

        Tick t ->
            let
                newClock =
                    toFloat <| posixToMillis t

                ( newAnim, newImages ) =
                    case model.mbAnim of
                        Just ( anim, AnimateLeft ) ->
                            if isDone newClock anim then
                                ( Nothing, right (.images model) )
                            else
                                ( model.mbAnim, model.images )

                        Just ( anim, AnimateRight ) ->
                            if isDone newClock anim then
                                ( Nothing, left (.images model) )
                            else
                                ( model.mbAnim, model.images )

                        Just ( anim, AlphaFade ) ->
                            if isDone newClock anim then
                                ( Nothing, left (.images model) )
                            else
                                ( model.mbAnim, model.images )

                        _ ->
                            ( model.mbAnim, model.images )
            in
            { model
                | clock = newClock
                , mbAnim = newAnim
                , images = newImages
            }

        VisibilityChange visibility ->
            { model | visibility = visibility }

        NoOp ->
            model


view : { maxWidth : Int } -> Model msg -> Element msg
view config model =
    Element.map model.externalMsg <|
        galleryView model config


galleryView model config =
    let
        maxWidth =
            min (config.maxWidth - 30)
                970
    in
    el
        [ centerX
        , clipX
        , width (px maxWidth)
        , height (px (round (toFloat maxWidth / 5)))

        --, paddingXY 15 0
        ]
        (chunkView model { maxWidth = maxWidth } (current model.images))


chunkView model config chunk =
    let
        frontOpacity =
            case model.mbAnim of
                Just ( anim, AlphaFade ) ->
                    alpha <| animate model.clock anim

                _ ->
                    alpha 1

        backOpacity =
            case model.mbAnim of
                Just ( anim, AlphaFade ) ->
                    alpha <| 1 - animate model.clock anim

                _ ->
                    alpha 0
    in
    case chunk of
        l :: c :: r :: [] ->
            Lazy.lazy
                (\mc ->
                    row
                        (events model.mbDrag ( DragStart, DragAt, DragEnd )
                            ++ [ mc ]
                        )
                        [ picView model config l []
                        , el
                            [ behindContent
                                (picView model config l [ backOpacity ])
                            ]
                            (picView model config c [ frontOpacity ])
                        , picView model config r []
                        ]
                )
                (moveChunk config model)

        _ ->
            Element.none


picView model config { src } attrs =
    if Set.member src model.loaded then
        column
            []
            [ el
                ([ width (px config.maxWidth)
                 , height (px (round (toFloat config.maxWidth / 5)))
                 , Background.image src
                 ]
                    ++ attrs
                    ++ unselectable
                )
                Element.none
            ]
    else
        column
            [ Background.color lightGrey
            , width (px config.maxWidth)
            , height (px (round (toFloat config.maxWidth / 5)))
            ]
            [ image
                [ centerX
                , centerY
                , clip
                ]
                { src = "/assets/images/loading.gif"
                , description = "chargement en cours"
                }
            , html <|
                Html.img
                    [ HtmlAttr.hidden True
                    , HtmlEvents.on "load" (Decode.succeed (ImgLoaded src))
                    , HtmlAttr.src src
                    ]
                    []
            ]


moveChunk config model =
    let
        animFun =
            case model.mbAnim of
                Just ( anim, AnimateLeft ) ->
                    moveLeft (toFloat config.maxWidth + animate model.clock anim)

                Just ( anim, AnimateRight ) ->
                    moveRight ((toFloat <| -1 * config.maxWidth) + animate model.clock anim)

                _ ->
                    -- necessary in order to center the row
                    moveLeft (toFloat config.maxWidth)
    in
    case model.mbDrag of
        Nothing ->
            animFun

        Just (Drag start stop) ->
            if start.x - stop.x <= 0 then
                moveRight (toFloat <| (-1 * config.maxWidth) + abs (start.x - stop.x))
            else
                moveLeft (toFloat <| config.maxWidth + start.x - stop.x)
