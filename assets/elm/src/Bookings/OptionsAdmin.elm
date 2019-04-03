module Bookings.OptionsAdmin exposing (..)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost, secureRequest)
import Bookings.BookingsShared exposing (..)
import Dict exposing (..)
import Dict.Extra exposing (find)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Http exposing (..)
import Internals.Helpers exposing (..)
import List.Extra exposing (swapAt)
import MultLang.MultLang exposing (..)


type alias Model msg =
    { selected : Maybe Int
    , options : Dict Int BookingOption
    , optionNameFr : Maybe String
    , optionNameEn : Maybe String
    , optionPrice : Maybe Float
    , outMsg : Msg -> msg
    }


type Msg
    = NewOption
    | DeleteOption Int
    | SwapUp Int
    | SwapDown Int
    | NameInputFr String
    | NameInputEn String
    | SetPrice String
    | SelectOption Int
    | SaveChanges
    | Cancel
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { selected = Nothing
      , options = Dict.empty
      , optionNameFr = Nothing
      , optionNameEn = Nothing
      , optionPrice = Nothing
      , outMsg = outMsg
      }
    , Cmd.none
    )


update : { a | logInfo : LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        NewOption ->
            case ( model.optionNameEn, model.optionNameFr, model.optionPrice ) of
                ( Just en, Just fr, Just p ) ->
                    let
                        newOption =
                            { name = MultLangStr en fr
                            , price = p
                            , key = en
                            , picked = False
                            }

                        newModel =
                            { model
                                | options =
                                    Dict.insert
                                        (nextId model.options)
                                        newOption
                                        model.options
                            }
                    in
                    ( newModel
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteOption n ->
            let
                newModel =
                    { model | options = Dict.remove n model.options }
            in
            ( newModel, Cmd.none )

        SwapUp id ->
            let
                newOptions =
                    Dict.values model.options
                        |> swapAt id (id - 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList

                newModel =
                    { model | options = newOptions }
            in
            ( newModel
            , Cmd.none
            )

        SwapDown id ->
            let
                newOptions =
                    Dict.values model.options
                        |> swapAt id (id + 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList

                newModel =
                    { model | options = newOptions }
            in
            ( newModel
            , Cmd.none
            )

        NameInputFr name ->
            ( { model
                | optionNameFr =
                    if name == "" then
                        Nothing
                    else
                        Just name
              }
            , Cmd.none
            )

        NameInputEn name ->
            ( { model
                | optionNameEn =
                    if name == "" then
                        Nothing
                    else
                        Just name
              }
            , Cmd.none
            )

        SetPrice price ->
            case String.toFloat price of
                Just p ->
                    ( { model | optionPrice = Just p }, Cmd.none )

                Nothing ->
                    ( { model | optionPrice = Nothing }, Cmd.none )

        SelectOption n ->
            case Dict.get n model.options of
                Nothing ->
                    ( model, Cmd.none )

                Just { name, price } ->
                    ( { model
                        | selected = Just n
                        , optionNameFr = Just name.fr
                        , optionNameEn = Just name.en
                        , optionPrice = Just price
                      }
                    , Cmd.none
                    )

        SaveChanges ->
            case ( model.selected, ( model.optionNameFr, model.optionNameEn ), model.optionPrice ) of
                ( Just n, ( Just fr, Just en ), Just p ) ->
                    let
                        newModel =
                            { model
                                | options =
                                    Dict.update n
                                        (\mbO ->
                                            case mbO of
                                                Nothing ->
                                                    Nothing

                                                Just o ->
                                                    Just
                                                        { o
                                                            | name =
                                                                MultLangStr en fr
                                                            , price = p
                                                        }
                                        )
                                        model.options
                                , selected = Nothing
                                , optionNameFr = Nothing
                                , optionNameEn = Nothing
                                , optionPrice = Nothing
                            }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model
                | selected = Nothing
                , optionNameFr = Nothing
                , optionNameEn = Nothing
                , optionPrice = Nothing
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


view config model =
    Element.none
