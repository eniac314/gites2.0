port module Bookings.BookingsAdmin exposing (..)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost)
import Bookings.BookingsShared exposing (..)
import Bookings.DatePicker.Date exposing (formatDate)
import Bookings.DatePicker.DatePicker as DP
import Date exposing (..)
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
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.PhoenixPresence as Presence exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import MultLang.MultLang exposing (..)
import Prng.Uuid as Uuid
import Style.Helpers exposing (noAttr, sides)
import Style.Palette exposing (..)


port presenceState : (Encode.Value -> msg) -> Sub msg


port presenceDiff : (Encode.Value -> msg) -> Sub msg


port receiveLockedDays : (Encode.Value -> msg) -> Sub msg


port receiveInitialLockedDays : (Encode.Value -> msg) -> Sub msg


subscriptions : Model msg -> Sub msg
subscriptions model =
    Sub.map model.outMsg <|
        Sub.batch
            [ receiveInitialLockedDays ReceiveInitialLockedDays
            , receiveLockedDays ReceiveLockedDays
            , presenceState ReceivePresenceState
            , presenceDiff ReceivePresenceDiff
            ]


type alias Model msg =
    { datePicker : DP.Model Msg
    , pickedDate : Maybe Date
    , presences : Presence.PresenceState String
    , availabilities : Dict Int DP.Availability
    , bookings : Dict Int BookingInfo
    , avLoadingStatus : Status
    , bookingLoadingStatus : Status
    , loadingStatus : Status
    , outMsg : Msg -> msg
    }


type Msg
    = DatePickerMsg DP.Msg
    | GetAvailabilities
    | ReceiveAvailabilities (Result Http.Error (Dict Int DP.Availability))
    | GetBookingInfo
    | ReceiveBookingInfo (Result Http.Error (List BookingInfo))
    | ReceiveInitialLockedDays Encode.Value
    | ReceiveLockedDays Encode.Value
    | ReceivePresenceState Decode.Value
    | ReceivePresenceDiff Decode.Value
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    let
        ( datePicker, datePickerCmd ) =
            DP.init Nothing DatePickerMsg
    in
    ( { datePicker = datePicker
      , pickedDate = Nothing
      , presences = Dict.empty
      , availabilities = Dict.empty
      , bookings = Dict.empty
      , bookingLoadingStatus = Initial
      , avLoadingStatus = Initial
      , loadingStatus = Initial
      , outMsg = outMsg
      }
    , Cmd.map outMsg <|
        Cmd.batch
            [ datePickerCmd
            ]
    )


load : { a | logInfo : LogInfo } -> Model msg -> ( Model msg, Cmd msg )
load config model =
    ( model
    , if model.loadingStatus == Initial then
        Cmd.map model.outMsg <|
            Cmd.batch
                [ getAvailabilities config.logInfo
                , getBookingInfo config.logInfo
                ]
      else
        Cmd.none
    )


loadingStatus bookingLoadingStatus avLoadingStatus =
    case ( bookingLoadingStatus, avLoadingStatus ) of
        ( Initial, Initial ) ->
            Initial

        ( Success, Success ) ->
            Success

        ( Failure, _ ) ->
            Failure

        ( _, Failure ) ->
            Failure

        _ ->
            Waiting


update : { a | logInfo : LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        DatePickerMsg dpMsg ->
            let
                ( datePicker, cmd, mbDate ) =
                    DP.update dpMsg model.datePicker
            in
            case mbDate of
                Nothing ->
                    ( { model | datePicker = datePicker }
                    , Cmd.map model.outMsg cmd
                    )

                Just pickedDate ->
                    ( { model
                        | pickedDate = Just pickedDate
                        , datePicker = datePicker
                      }
                    , Cmd.map model.outMsg cmd
                    )

        GetAvailabilities ->
            ( { model | loadingStatus = Waiting }
            , Cmd.map model.outMsg <|
                getAvailabilities config.logInfo
            )

        ReceiveInitialLockedDays json ->
            case Decode.decodeValue (Decode.field "payload" <| Decode.list lockedDaysDecoder) json of
                Ok lDays ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceiveLockedDays json ->
            case Decode.decodeValue lockedDaysDecoder json of
                Ok { cIn, cOut, uuid } ->
                    let
                        availabilities =
                            List.foldr
                                (\d acc ->
                                    Dict.insert (Date.toRataDie d) DP.Locked acc
                                )
                                model.availabilities
                                (daysBooked cIn cOut)
                    in
                    ( { model | availabilities = availabilities }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GetBookingInfo ->
            ( model, Cmd.none )

        ReceiveBookingInfo res ->
            case res of
                Ok bookings ->
                    ( { model
                        | bookings =
                            List.map (\b -> ( b.bookingId, b ))
                                bookings
                                |> Dict.fromList
                        , bookingLoadingStatus = Success
                        , loadingStatus = loadingStatus Success model.avLoadingStatus
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model
                        | bookingLoadingStatus = Failure
                        , loadingStatus = Failure
                      }
                    , Cmd.none
                    )

        ReceiveAvailabilities res ->
            case res of
                Ok avs ->
                    ( { model
                        | availabilities = avs
                        , avLoadingStatus = Success
                        , loadingStatus = loadingStatus model.bookingLoadingStatus Success
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | avLoadingStatus = Failure
                        , loadingStatus = Failure
                      }
                    , Cmd.none
                    )

        ReceivePresenceState jsonVal ->
            let
                presences =
                    decodePresenceState jsonVal
                        |> Result.map (\state -> Presence.syncState state model.presences)
            in
            case presences of
                Ok ps ->
                    ( { model
                        | presences = ps
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

        ReceivePresenceDiff jsonVal ->
            let
                presences =
                    decodePresenceDiff jsonVal
                        |> Result.map (\diff -> Presence.syncDiff diff model.presences)
            in
            case presences of
                Ok ps ->
                    ( { model
                        | presences = ps
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type alias ViewConfig =
    { lang : Lang
    , width : Int
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ width fill
            , height fill
            , paddingEach { top = 45, right = 45, bottom = 45, left = 45 }
            , spacing 30
            , Background.color lightGrey
            ]
            [ datePickerView config model
            , bookingListView config model
            ]


datePickerView : { a | lang : Lang } -> Model msg -> Element Msg
datePickerView config model =
    column
        [ spacing 15
        , width fill
        ]
        [ el [ Font.bold ]
            (textM config.lang
                (MultLangStr "Booking and availability calendar"
                    "Calendrier des réservations et disponibilités"
                )
            )
        , column
            [ spacing 15 ]
            [ row
                [ spacing 15 ]
                [ row
                    [ spacing 10 ]
                    [ el
                        [ width (px 15)
                        , height (px 15)
                        , Border.color grey
                        , Border.width 1
                        , Background.color calGreen
                        ]
                        Element.none
                    , el []
                        (textM config.lang
                            (MultLangStr "Available"
                                "Libre"
                            )
                        )
                    ]
                , row
                    [ spacing 10 ]
                    [ el
                        [ width (px 15)
                        , height (px 15)
                        , Border.color grey
                        , Border.width 1
                        , Background.color calRed
                        ]
                        Element.none
                    , el []
                        (textM config.lang
                            (MultLangStr "Not available"
                                "Indisponible"
                            )
                        )
                    ]
                ]
            , row
                [ spacing 10 ]
                [ row
                    [ spacing 10 ]
                    [ el
                        [ width (px 15)
                        , height (px 15)
                        , Border.color grey
                        , Border.width 1
                        , Background.color calOrange
                        ]
                        Element.none
                    , el []
                        (textM config.lang
                            (MultLangStr "Booked"
                                "Réservé"
                            )
                        )
                    ]
                , row
                    [ spacing 10 ]
                    [ el
                        [ width (px 15)
                        , height (px 15)
                        , Border.color grey
                        , Border.width 1
                        , Background.color darkGreen
                        ]
                        Element.none
                    , el []
                        (textM config.lang
                            (MultLangStr "Locked"
                                "Vérouillé"
                            )
                        )
                    ]
                ]
            , row
                [ spacing 10 ]
                [ el
                    [ width (px 15)
                    , height (px 15)
                    , Border.color grey
                    , Border.width 1
                    , Background.color blue
                    ]
                    Element.none
                , el []
                    (textM config.lang
                        (MultLangStr "Available but not as checkin date"
                            "Libre mais pas comme date d'arrivée"
                        )
                    )
                ]
            , row
                [ spacing 10 ]
                [ el
                    [ width (px 15)
                    , height (px 15)
                    , Border.color grey
                    , Border.width 1
                    , Background.color purple
                    ]
                    Element.none
                , el []
                    (textM config.lang
                        (MultLangStr "Available but not as checkout date"
                            "Libre mais pas comme date de départ"
                        )
                    )
                ]
            ]
        , DP.view
            { lang = config.lang
            , availability = checkAvailability model
            , pickedDate = model.pickedDate
            }
            model.datePicker
        ]


bookingListView config model =
    let
        headerStyle =
            [ Background.color white
            , paddingXY 10 10
            , Font.center
            , Border.color grey
            , Border.widthEach
                { sides
                    | bottom = 1
                    , right = 1
                }
            ]

        cellStyle i =
            [ if modBy 2 i /= 0 then
                Background.color white
              else
                noAttr
            , paddingXY 0 5
            , Font.center
            , Border.color grey
            , Border.widthEach
                { sides
                    | bottom = 1
                    , right = 1
                }
            ]

        bookings =
            Dict.values model.bookings
    in
    indexedTable
        [ Border.color grey
        , Border.widthEach
            { sides
                | top = 1
                , left = 1
            }
        , width (px 500)
        ]
        { data = bookings
        , columns =
            [ { header =
                    el
                        headerStyle
                        (textM config.lang
                            (MultLangStr "Check-in" "Date d'arrivée")
                        )
              , width = fill
              , view =
                    \i bi ->
                        el (cellStyle i) (text <| formatDate config.lang bi.checkIn)
              }
            , { header =
                    el
                        headerStyle
                        (textM config.lang
                            (MultLangStr "Check-out" "Date de départ")
                        )
              , width = fill
              , view =
                    \i bi ->
                        el (cellStyle i) (text <| formatDate config.lang bi.checkOut)
              }
            , { header =
                    el
                        headerStyle
                        (textM config.lang
                            (MultLangStr "Customer name" "Nom client")
                        )
              , width = fill
              , view =
                    \i bi ->
                        el (cellStyle i) (text <| bi.firstName ++ " " ++ bi.lastName)
              }
            , { header =
                    el
                        headerStyle
                        (textM config.lang
                            (MultLangStr "Confirmed" "Confirmée")
                        )
              , width = fill
              , view =
                    \i bi ->
                        el (cellStyle i)
                            (if bi.confirmed then
                                el
                                    [ Font.color green
                                    , centerX
                                    ]
                                    (textM config.lang (MultLangStr "yes" "oui"))
                             else
                                el
                                    [ Font.color red
                                    , centerX
                                    ]
                                    (textM config.lang (MultLangStr "no" "non"))
                            )
              }
            ]
        }


checkAvailability : Model msg -> (Date -> DP.Availability)
checkAvailability { availabilities } =
    \d ->
        Dict.get (Date.toRataDie d) availabilities
            |> Maybe.withDefault DP.Available



-------------------------------------------------------------------------------
----------------------------
-- Http and Json Handling --
----------------------------


getAvailabilities logInfo =
    secureGet logInfo
        { url = "/api/restricted/availabilities"
        , expect =
            Http.expectJson
                ReceiveAvailabilities
                decodeAvailabilities
        }


decodeAvailabilities : Decode.Decoder (Dict Int DP.Availability)
decodeAvailabilities =
    Decode.field "data"
        (Decode.list decodeAvailability
            |> Decode.map Dict.fromList
        )


decodeAvailability : Decode.Decoder ( Int, DP.Availability )
decodeAvailability =
    Decode.map3 (\d av id -> ( d, av, id ))
        (Decode.field "date" Decode.int)
        (Decode.field "availability"
            (Decode.string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "NotAvailable" ->
                                Decode.succeed DP.NotAvailable

                            "NoCheckIn" ->
                                Decode.succeed DP.NoCheckInAdmin

                            "NoCheckOut" ->
                                Decode.succeed DP.NoCheckOutAdmin

                            "Booked" ->
                                Decode.succeed (DP.BookedAdmin 0)

                            somethingElse ->
                                Decode.fail <|
                                    "Unknown CellContent: "
                                        ++ somethingElse
                    )
            )
        )
        (Decode.field "bookingId" (Decode.nullable Decode.int))
        |> Decode.map
            (\( d, av, mbId ) ->
                case ( av, mbId ) of
                    ( DP.BookedAdmin _, Just id ) ->
                        ( d, DP.BookedAdmin id )

                    _ ->
                        ( d, av )
            )


getBookingInfo logInfo =
    secureGet logInfo
        { url = "/api/restricted/bookings/"
        , expect =
            Http.expectJson
                ReceiveBookingInfo
                (Decode.field "data" <| Decode.list decodeBookingInfo)
        }
