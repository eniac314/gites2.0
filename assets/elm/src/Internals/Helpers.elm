module Internals.Helpers exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Http exposing (Error(..))
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
    "https://s3.eu-west-3.amazonaws.com/gite-vieux-lilas/"
