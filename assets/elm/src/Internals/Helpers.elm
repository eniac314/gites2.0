module Internals.Helpers exposing
    ( Log
    , PluginResult(..)
    , Status(..)
    , awsUrl
    , bestFit
    , break
    , chunk
    , chunkedRows
    , decoBorder
    , decodeImageMeta
    , decodeMls
    , decodeSize
    , encodeImageMeta
    , encodeMls
    , encodeSize
    , getArtworks
    , httpErrorToString
    , logsView
    , newLog
    , nextId
    , thumbSrc
    )

import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Random exposing (Seed, initialSeed, int, list, step)
import String.Extra exposing (insertAt)
import Style.Helpers exposing (..)
import Task exposing (perform)
import Time exposing (Posix)


type PluginResult a
    = PluginQuit
    | PluginData a


type Status
    = Initial
    | Waiting
    | Success
    | Failure



-------------------------------------------------------------------------------
------------
-- Logger --
------------


type alias Log =
    { message : String
    , mbDetails : Maybe String
    , isError : Bool
    , timeStamp : Posix
    }


newLog : (Log -> msg) -> String -> Maybe String -> Bool -> Cmd msg
newLog addLogMsg logMsg details isError =
    Task.perform addLogMsg <|
        (Time.now
            |> Task.andThen
                (\t ->
                    Task.succeed <|
                        Log logMsg
                            details
                            isError
                            t
                )
        )


logsView : List Log -> Time.Zone -> Element msg
logsView logs zone =
    let
        formatTime =
            String.fromInt
                >> String.padLeft 2 '0'

        logView { message, mbDetails, isError, timeStamp } =
            column
                [ spacing 5
                , width (maximum 500 fill)
                ]
                [ row [ spacing 15 ]
                    [ el [ Font.color (rgb 0.7 0.7 0.7) ]
                        (text <|
                            formatTime (Time.toHour zone timeStamp)
                                ++ ":"
                                ++ formatTime (Time.toMinute zone timeStamp)
                        )
                    , el
                        [ if isError then
                            Font.color (rgb 1 0 0)

                          else
                            noAttr
                        ]
                        (text message)
                    ]
                , case mbDetails of
                    Nothing ->
                        Element.none

                    Just details ->
                        paragraph
                            [ paddingEach
                                { top = 0
                                , bottom = 0
                                , left = 20
                                , right = 0
                                }
                            , Font.size 12
                            ]
                            [ text details ]
                ]
    in
    column [ spacing 15 ]
        (List.map logView logs)



-------------------------------------------------------------------------------
----------
-- Misc --
----------


chunkedRows : Int -> (Int -> Int) -> List (Element msg) -> List (Element msg)
chunkedRows width chunkBy elems =
    let
        nbrChunks =
            chunkBy width

        chunks =
            chunk nbrChunks elems
    in
    List.map
        (row
            [ centerX
            , spacing 10
            ]
        )
        chunks


chunk : Int -> List a -> List (List a)
chunk n xs =
    let
        helper acc ys =
            case ys of
                [] ->
                    List.reverse acc

                _ ->
                    helper (List.take n ys :: acc) (List.drop n ys)
    in
    helper [] xs


break : (a -> Bool) -> List a -> ( List a, List a )
break p xs =
    let
        helper ys left =
            case ys of
                [] ->
                    ( left, [] )

                y :: ys_ ->
                    if p y then
                        ( List.reverse left, y :: ys_ )

                    else
                        helper ys_ (y :: left)
    in
    helper xs []


bestFit : Int -> Int -> Int
bestFit elemWidth width =
    let
        nbrChunks_ =
            width // elemWidth

        spacing =
            (nbrChunks_ - 1) * 15
    in
    if (nbrChunks_ * elemWidth + spacing) < width then
        nbrChunks_

    else
        nbrChunks_ - 1


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        BadUrl s ->
            "Url invalide: " ++ s

        Timeout ->
            "Délai d'attente dépassé"

        NetworkError ->
            "Erreur de réseau"

        BadStatus statusCode ->
            "Erreur serveur: "
                ++ String.fromInt statusCode

        BadBody details ->
            "Erreur décodage: " ++ details


awsUrl =
    "https://gite-vieux-lilas.s3.eu-west-3.amazonaws.com/"


thumbSrc : String -> String
thumbSrc s =
    case List.reverse <| String.indexes "/" s of
        [] ->
            s

        n :: _ ->
            String.Extra.insertAt "/thumbs" n s


encodeMls : MultLangStr -> E.Value
encodeMls { en, fr } =
    E.object
        [ ( "MultLangStr"
          , E.object
                [ ( "en", E.string en )
                , ( "fr", E.string fr )
                ]
          )
        ]


encodeImageMeta : ImageMeta -> E.Value
encodeImageMeta { url, caption, size } =
    E.object
        [ ( "url", E.string url )
        , ( "caption"
          , Maybe.map encodeMls caption
                |> Maybe.withDefault E.null
          )
        , ( "size", encodeSize size )
        ]


encodeSize : { width : Int, height : Int } -> E.Value
encodeSize size =
    E.object
        [ ( "width", E.int size.width )
        , ( "height", E.int size.height )
        ]


decodeMls : D.Decoder MultLangStr
decodeMls =
    D.field
        "MultLangStr"
    <|
        D.map2 MultLangStr
            (D.field "en" D.string)
            (D.field "fr" D.string)


decodeImageMeta : D.Decoder ImageMeta
decodeImageMeta =
    D.map3 ImageMeta
        (D.field "url" D.string)
        (D.field "caption" (D.nullable decodeMls))
        (D.field "size" decodeSize)


decodeSize : D.Decoder { width : Int, height : Int }
decodeSize =
    D.map2 (\w h -> { width = w, height = h })
        (D.field "width" D.int)
        (D.field "height" D.int)


nextId : Dict Int a -> Int
nextId dict =
    1
        + Dict.foldr
            (\k v acc ->
                max k acc
            )
            0
            dict


shuffle : Random.Seed -> List a -> List a
shuffle seed xs =
    let
        n =
            List.length xs

        ( randlist, _ ) =
            Random.step (Random.list n (Random.float 0 1)) seed
    in
    List.map2 Tuple.pair randlist xs
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


getArtworks : Int -> ( String, String, String )
getArtworks time =
    case shuffle (Random.initialSeed time) artwork of
        a :: b :: c :: xs ->
            ( a, b, c )

        _ ->
            ( "", "", "" )


artwork =
    [ "/images/chair.png"
    , "/images/clock.png"
    , "/images/duck.png"
    , "/images/piano.png"
    , "/images/bigClock.png"
    , "/images/washingTable.png"
    ]


decoBorder =
    "/images/DecorativeBorder.png"
