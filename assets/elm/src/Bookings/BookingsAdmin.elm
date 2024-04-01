port module Bookings.BookingsAdmin exposing (Model, Msg(..), ViewConfig, avStr, bookingEditorView, bookingListView, bookingView, broadcastRefresh, checkAvailability, confirmBooking, confirmationMail, datePickerView, decodeAvailabilities, decodeAvailability, deleteBooking, editAvailabilityView, encodeAvailability, getAvailabilities, getBookingInfo, init, load, loadingStatus, presenceDiff, presenceState, presenceStateView, rangeAvailability, receiveInitialLockedDays, receiveLockedDays, setAvailability, subscriptions, update, view)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost, secureRequest)
import Bookings.BookingsShared exposing (..)
import Bookings.DatePicker.Date exposing (formatDate)
import Bookings.DatePicker.DatePicker as DP
import Browser.Events exposing (Visibility(..), onKeyDown, onKeyUp, onVisibilityChange)
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
import Html.Events exposing (keyCode)
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.PhoenixPresence as Presence exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import MultLang.MultLang exposing (..)
import PdfGen.Contrat as Contrat
import Prng.Uuid as Uuid
import Style.Helpers exposing (buttonStyle, noAttr, sides)
import Style.Palette exposing (..)


port presenceState : (Encode.Value -> msg) -> Sub msg


port presenceDiff : (Encode.Value -> msg) -> Sub msg


port receiveLockedDays : (Encode.Value -> msg) -> Sub msg


port broadcastRefresh : (Encode.Value -> msg) -> Sub msg


port receiveInitialLockedDays : (Encode.Value -> msg) -> Sub msg


subscriptions : Model msg -> Sub msg
subscriptions model =
    Sub.map model.outMsg <|
        Sub.batch
            [ receiveInitialLockedDays ReceiveInitialLockedDays
            , receiveLockedDays ReceiveLockedDays
            , broadcastRefresh (\_ -> Refresh)
            , presenceState ReceivePresenceState
            , presenceDiff ReceivePresenceDiff
            , onKeyDown (Decode.map KeyDown keyCode)
            , onKeyUp (Decode.map KeyUp keyCode)
            ]


type alias Model msg =
    { datePicker : DP.Model Msg
    , pickedDate : Maybe Date
    , pickedBooking : Maybe BookingInfo
    , pickedAvailability : Maybe DP.Availability
    , rowHovered : Maybe Int
    , presences : Presence.PresenceState String
    , availabilities : Dict Int DP.Availability
    , bookings : Dict Int BookingInfo
    , avLoadingStatus : Status
    , bookingLoadingStatus : Status
    , loadingStatus : Status
    , commentReply : Maybe String
    , ctrlDown : Bool
    , rangeStart : Maybe Date
    , outMsg : Msg -> msg
    }


type Msg
    = DatePickerMsg DP.Msg
    | PickBooking Int
    | RowHovered (Maybe Int)
    | GetAvailabilities
    | ReceiveAvailabilities (Result Http.Error (Dict Int DP.Availability))
    | Refresh
    | ConfirmBooking BookingInfo
    | CommentReplyInput String
    | BookingConfirmed Int (Result Http.Error ())
    | DeleteBooking Int
    | BookingDeleted Int (Result Http.Error ())
    | ReceiveBookingInfo (Result Http.Error (List BookingInfo))
    | SetAvailability Date DP.Availability
    | SetRangeAvailability DP.Availability
    | AvailabilitySet Date (Result Http.Error ())
    | ReceiveInitialLockedDays Encode.Value
    | ReceiveLockedDays Encode.Value
    | ReceivePresenceState Decode.Value
    | ReceivePresenceDiff Decode.Value
    | KeyDown Int
    | KeyUp Int
    | VisibilityChange Visibility
    | SaveContratPdf
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    let
        ( datePicker, datePickerCmd ) =
            DP.init Nothing True DatePickerMsg
    in
    ( { datePicker = datePicker
      , pickedDate = Nothing
      , pickedBooking = Nothing
      , pickedAvailability = Nothing
      , rowHovered = Nothing
      , presences = Dict.empty
      , availabilities = Dict.empty
      , bookings = Dict.empty
      , bookingLoadingStatus = Initial
      , avLoadingStatus = Initial
      , loadingStatus = Initial
      , commentReply = Nothing
      , ctrlDown = False
      , outMsg = outMsg
      , rangeStart = Nothing
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
                    let
                        currentAv =
                            Dict.get (Date.toRataDie pickedDate) model.availabilities
                    in
                    ( { model
                        | pickedDate = Just pickedDate
                        , rangeStart =
                            if model.ctrlDown then
                                model.pickedDate

                            else
                                Nothing
                        , datePicker = datePicker
                        , pickedBooking =
                            currentAv
                                |> Maybe.andThen
                                    (\av ->
                                        case av of
                                            DP.BookedAdmin id ->
                                                Dict.get id model.bookings

                                            _ ->
                                                Nothing
                                    )
                        , pickedAvailability =
                            case currentAv of
                                Just ac ->
                                    currentAv

                                _ ->
                                    Just DP.Available
                      }
                    , Cmd.map model.outMsg cmd
                    )

        PickBooking id ->
            ( { model | pickedBooking = Dict.get id model.bookings }
            , Cmd.none
            )

        RowHovered mbInd ->
            ( { model | rowHovered = mbInd }
            , Cmd.none
            )

        GetAvailabilities ->
            ( { model | loadingStatus = Waiting }
            , Cmd.map model.outMsg <|
                getAvailabilities config.logInfo
            )

        ReceiveInitialLockedDays json ->
            case Decode.decodeValue (Decode.field "payload" <| Decode.list lockedDaysDecoder) json of
                Ok lDays ->
                    let
                        availabilities =
                            List.foldr
                                (\{ cIn, cOut, uuid } acc ->
                                    List.foldr
                                        (\d acc_ ->
                                            Dict.insert (Date.toRataDie d) (DP.Locked uuid) acc_
                                        )
                                        acc
                                        (daysBooked cIn cOut)
                                )
                                model.availabilities
                                lDays
                    in
                    ( { model | availabilities = availabilities }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ReceiveLockedDays json ->
            case Decode.decodeValue lockedDaysDecoder json of
                Ok { cIn, cOut, uuid } ->
                    let
                        availabilities =
                            List.foldr
                                (\d acc ->
                                    Dict.insert (Date.toRataDie d) (DP.Locked uuid) acc
                                )
                                model.availabilities
                                (daysBooked cIn cOut)
                    in
                    ( { model | availabilities = availabilities }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Refresh ->
            ( model
            , Cmd.map model.outMsg <|
                Cmd.batch
                    [ getAvailabilities config.logInfo
                    , getBookingInfo config.logInfo
                    ]
            )

        ConfirmBooking bookingInfo ->
            ( model
            , Cmd.map model.outMsg <|
                confirmBooking config.logInfo model.commentReply bookingInfo
            )

        CommentReplyInput s ->
            ( { model | commentReply = Just s }
            , Cmd.none
            )

        BookingConfirmed id res ->
            case res of
                Ok () ->
                    ( model
                    , Cmd.map model.outMsg <| getBookingInfo config.logInfo
                    )

                Err _ ->
                    ( model, Cmd.none )

        DeleteBooking id ->
            ( model
            , Cmd.map model.outMsg <|
                deleteBooking config.logInfo id
            )

        BookingDeleted id res ->
            case res of
                Ok () ->
                    ( { model | pickedBooking = Nothing }
                    , Cmd.map model.outMsg <|
                        Cmd.batch
                            [ getBookingInfo config.logInfo
                            , getAvailabilities config.logInfo
                            ]
                    )

                Err _ ->
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
                        | availabilities =
                            Dict.union
                                avs
                                (Dict.filter
                                    (\k v ->
                                        case v of
                                            DP.Locked _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    model.availabilities
                                )
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

        SetAvailability d av ->
            ( { model | pickedAvailability = Just av }
            , Cmd.map model.outMsg <|
                setAvailability config.logInfo d av
            )

        SetRangeAvailability av ->
            case ( model.rangeStart, model.pickedDate ) of
                ( Just start, Just end ) ->
                    let
                        daysToSet =
                            daysBooked start (Date.add Date.Days 1 end)
                                |> List.filter
                                    (\d ->
                                        case Dict.get (toRataDie d) model.availabilities of
                                            Just (DP.BookedAdmin _) ->
                                                False

                                            Just (DP.Locked _) ->
                                                False

                                            _ ->
                                                True
                                    )
                    in
                    ( { model | pickedAvailability = Just av }
                    , List.map (\d -> setAvailability config.logInfo d av) daysToSet
                        |> Cmd.batch
                        |> Cmd.map model.outMsg
                    )

                _ ->
                    ( model, Cmd.none )

        AvailabilitySet d res ->
            ( model
            , Cmd.map model.outMsg <|
                Cmd.batch
                    [ getAvailabilities config.logInfo
                    ]
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
                        , availabilities =
                            Dict.filter
                                (\k v ->
                                    case v of
                                        DP.Locked uuid ->
                                            List.member uuid (Dict.keys ps)

                                        _ ->
                                            True
                                )
                                model.availabilities
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

        KeyDown kc ->
            ( { model | ctrlDown = kc == 17 }, Cmd.none )

        KeyUp kc ->
            ( { model
                | ctrlDown =
                    if kc == 17 then
                        False

                    else
                        model.ctrlDown
                , rangeStart =
                    if kc == 17 then
                        Nothing

                    else
                        model.rangeStart
              }
            , Cmd.none
            )

        VisibilityChange v ->
            ( { model
                | ctrlDown =
                    if v == Hidden then
                        False

                    else
                        model.ctrlDown
                , rangeStart =
                    if v == Hidden then
                        Nothing

                    else
                        model.rangeStart
              }
            , Cmd.none
            )

        SaveContratPdf ->
            ( model, Contrat.savePdf "contratPDF" )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


type alias ViewConfig =
    { lang : Lang
    , width : Int
    , options : BookingOptions
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ width fill
            , height fill
            , spacing 20
            ]
            [ row
                [ width fill
                , height fill
                , paddingEach { top = 45, right = 45, bottom = 45, left = 45 }
                , spacing 30
                , Background.color lightGrey
                ]
                [ column
                    [ spacing 20
                    , alignTop
                    ]
                    [ datePickerView config model
                    , bookingListView config model
                    ]
                , column
                    [ spacing 20
                    , alignTop
                    , width fill
                    ]
                    [ presenceStateView config model
                    , bookingEditorView config model
                    ]
                ]
            , html <| Contrat.view config model
            ]


datePickerView : { a | lang : Lang } -> Model msg -> Element Msg
datePickerView config model =
    column
        [ spacing 15
        , width (px 630)
        , Background.color white
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
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
                    , Background.color lightBlue
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
        , row
            [ spacing 10 ]
            [ DP.view
                { lang = config.lang
                , availability = checkAvailability model
                , pickedDate = model.pickedDate
                }
                model.datePicker
            , case model.pickedDate of
                Just pd ->
                    if model.ctrlDown then
                        case model.rangeStart of
                            Just start ->
                                row
                                    [ spacing 10 ]
                                    [ el [ Font.bold ]
                                        (textM config.lang
                                            (MultLangStr "Date range:"
                                                "Intervalle:"
                                            )
                                        )
                                    , text <| formatDate config.lang start
                                    , text "-"
                                    , text <| formatDate config.lang pd
                                    ]

                            Nothing ->
                                row
                                    [ spacing 10 ]
                                    [ el [ Font.bold ]
                                        (textM config.lang
                                            (MultLangStr "Range start:"
                                                "Début intervalle:"
                                            )
                                        )
                                    , text <| formatDate config.lang pd
                                    ]

                    else
                        el
                            [ Font.italic ]
                            (textM config.lang
                                (MultLangStr "Hold Ctrl to pick a date range"
                                    "Maintenir Ctrl pour choisir un intervalle"
                                )
                            )

                Nothing ->
                    Element.none
            ]
        ]


bookingListView config model =
    let
        headerStyle =
            [ Background.color lightGrey
            , paddingXY 10 10
            , Font.center
            , Border.color grey
            , Border.widthEach
                { sides
                    | bottom = 1
                    , right = 1
                }
            ]

        cellStyle id i =
            [ if modBy 2 i /= 0 then
                Background.color lightGrey

              else
                noAttr
            , Events.onMouseEnter (RowHovered <| Just i)
            , Events.onMouseLeave (RowHovered Nothing)
            , if model.rowHovered == Just i then
                Background.color grey

              else
                noAttr
            , Events.onClick (PickBooking id)
            , pointer
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
    column
        [ Background.color white
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        , height (maximum 400 fill)
        , scrollbarY
        , spacing 15
        ]
        [ el [ Font.bold ]
            (textM config.lang
                (MultLangStr "Booking list"
                    "Liste des réservations"
                )
            )
        , indexedTable
            [ Border.color grey
            , Border.widthEach
                { sides
                    | top = 1
                    , left = 1
                }
            , width (px 600)
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
                            el (cellStyle bi.bookingId i) (text <| formatDate config.lang bi.checkIn)
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
                            el (cellStyle bi.bookingId i) (text <| formatDate config.lang bi.checkOut)
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
                            el (cellStyle bi.bookingId i) (text <| bi.firstName ++ " " ++ bi.lastName)
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
                            el (cellStyle bi.bookingId i)
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
        ]


checkAvailability : Model msg -> (Date -> DP.Availability)
checkAvailability { availabilities, ctrlDown, rangeStart, pickedDate } =
    if ctrlDown then
        rangeAvailability availabilities rangeStart pickedDate

    else
        \d ->
            Dict.get (Date.toRataDie d) availabilities
                |> Maybe.withDefault DP.Available


rangeAvailability : Dict Int DP.Availability -> Maybe Date -> Maybe Date -> (Date -> DP.Availability)
rangeAvailability availabilities rangeStart pickedDate =
    case ( rangeStart, pickedDate ) of
        ( Just start, Just end ) ->
            let
                newAvailabilities =
                    List.foldr
                        (\d acc -> Dict.insert (Date.toRataDie d) DP.Booked acc)
                        availabilities
                        (daysBooked start end)
            in
            \d ->
                Dict.get (Date.toRataDie d) newAvailabilities
                    |> Maybe.withDefault DP.Available

        ( Nothing, Just pd ) ->
            let
                newAvailabilities =
                    Dict.map
                        (\k av ->
                            case av of
                                DP.BookedAdmin _ ->
                                    DP.NotAvailableAdmin

                                DP.Locked _ ->
                                    DP.NotAvailableAdmin

                                otherwise ->
                                    otherwise
                        )
                        availabilities
            in
            \d ->
                if
                    (Date.compare d (Date.add Days -1 pd) == LT)
                        || (Date.compare d (Date.add Days -1 pd) == EQ)
                then
                    DP.NotAvailable

                else
                    Dict.get (Date.toRataDie d) newAvailabilities
                        |> Maybe.withDefault DP.Available

        _ ->
            always DP.NotAvailable


presenceStateView config model =
    let
        n =
            Dict.size model.presences - 1

        isPlural =
            n > 1

        nbrUsers lang =
            strM
                lang
                (MultLangStr
                    (String.fromInt n
                        ++ (if isPlural then
                                " users"

                            else
                                " user"
                           )
                    )
                    (String.fromInt n
                        ++ (if isPlural then
                                " utilisateurs"

                            else
                                " utilisateur"
                           )
                    )
                )
    in
    column
        [ Background.color white
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        , spacing 15
        , width fill
        ]
        [ el [ Font.bold ]
            (textM config.lang
                (MultLangStr "Real-time application usage"
                    "Utilisation temps réel"
                )
            )
        , paragraph
            []
            [ textM config.lang
                (MultLangStr
                    ("There "
                        ++ (if isPlural then
                                "are"

                            else
                                "is"
                           )
                        ++ " currently "
                        ++ nbrUsers config.lang
                        ++ " online."
                    )
                    ("Il y a actuellement "
                        ++ nbrUsers config.lang
                        ++ " en ligne."
                    )
                )
            ]
        ]


bookingEditorView config model =
    column
        [ spacing 15
        , width fill
        ]
        [ case ( model.pickedDate, model.pickedBooking ) of
            ( _, Just bookingInfo ) ->
                bookingView config bookingInfo model

            ( Just date, _ ) ->
                editAvailabilityView config model

            _ ->
                Element.none
        ]


bookingView config bookingInfo model =
    column
        [ Background.color white
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        , spacing 15
        , width fill
        ]
        [ customerDetailView config bookingInfo
        , contactView config bookingInfo
        , recapView config bookingInfo.checkIn bookingInfo.checkOut bookingInfo config.options
        , Input.multiline
            [ height (px 100)
            , width (px 400)
            ]
            { onChange = CommentReplyInput
            , text =
                model.commentReply
                    |> Maybe.withDefault ""
            , placeholder =
                Just <|
                    Input.placeholder
                        []
                        (textM config.lang
                            (MultLangStr "Remarks"
                                "Remarques"
                            )
                        )
            , spellcheck = False
            , label =
                Input.labelHidden ""
            }
        , row
            [ spacing 15
            , padding 15
            , Background.color lightGrey
            , Border.rounded 5
            , Border.color grey
            , Border.width 1
            , width fill
            ]
            [ Input.button
                (buttonStyle (not bookingInfo.confirmed))
                { onPress =
                    if bookingInfo.confirmed then
                        Nothing

                    else
                        Just (ConfirmBooking bookingInfo)
                , label =
                    textM config.lang
                        (MultLangStr "Confirm booking"
                            "Confirmer la réservation"
                        )
                }
            , Input.button
                (buttonStyle True)
                { onPress =
                    Just (DeleteBooking bookingInfo.bookingId)
                , label =
                    textM config.lang
                        (MultLangStr "Delete booking"
                            "Effacer la réservation"
                        )
                }
            , Input.button
                (buttonStyle True)
                { onPress =
                    Just SaveContratPdf
                , label =
                    textM config.lang
                        (MultLangStr "Generate contract"
                            "Générer Contrat"
                        )
                }
            ]
        ]


editAvailabilityView config model =
    let
        availability =
            case
                Maybe.andThen (\d -> Dict.get (Date.toRataDie d) model.availabilities)
                    model.pickedDate
            of
                Just av ->
                    av

                _ ->
                    DP.Available

        pDateStr =
            model.pickedDate
                |> Maybe.withDefault (Date.fromRataDie 0)
                |> formatDate config.lang

        rangeEdit =
            case ( model.rangeStart, model.pickedDate ) of
                ( Just _, Just _ ) ->
                    True

                _ ->
                    False
    in
    column
        [ Background.color white
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        , spacing 15
        , width fill
        ]
        [ el [ Font.bold ]
            (textM config.lang
                (if rangeEdit then
                    MultLangStr "Change availability - Range"
                        "Modification de la disponibilité - Intervalle"

                 else
                    MultLangStr "Change availability"
                        "Modification de la disponibilité"
                )
            )
        , if rangeEdit then
            Element.none

          else
            column
                [ spacing 10 ]
                [ textM config.lang
                    (MultLangStr "Current availability:"
                        "Disponibilité actuelle:\n"
                    )
                , el
                    [ Font.bold ]
                    (text <| strM config.lang (avStr availability))
                ]
        , if rangeEdit then
            Element.none

          else
            textM config.lang
                (MultLangStr
                    ("New availability on: "
                        ++ pDateStr
                    )
                    ("Nouvelle disponibilité pour le: "
                        ++ pDateStr
                    )
                )
        , Input.radio
            [ spacing 10 ]
            { onChange =
                if rangeEdit then
                    SetRangeAvailability

                else
                    Maybe.map
                        SetAvailability
                        model.pickedDate
                        |> Maybe.withDefault (\_ -> NoOp)
            , selected = model.pickedAvailability
            , label = Input.labelHidden ""
            , options =
                [ Input.option DP.Available (textM config.lang <| avStr DP.Available)
                , Input.option DP.NotAvailableAdmin (textM config.lang <| avStr DP.NotAvailableAdmin)
                , Input.option DP.NoCheckInAdmin (textM config.lang <| avStr DP.NoCheckInAdmin)
                , Input.option DP.NoCheckOutAdmin (textM config.lang <| avStr DP.NoCheckOutAdmin)
                ]
            }
        ]


avStr av =
    case av of
        DP.NotAvailableAdmin ->
            MultLangStr "Not available"
                "Indisponible"

        DP.NoCheckInAdmin ->
            MultLangStr "Available but not as checkin date"
                "Libre mais pas comme date d'arrivée"

        DP.NoCheckOutAdmin ->
            MultLangStr "Available but not as chekout date"
                "Libre mais pas comme date de départ"

        DP.BookedAdmin _ ->
            MultLangStr "Booked"
                "Réservé"

        _ ->
            MultLangStr "Available"
                "Libre"



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
                                Decode.succeed DP.NotAvailableAdmin

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


deleteBooking logInfo id =
    secureRequest logInfo
        { method = "DELETE"
        , headers = []
        , url = "api/restricted/bookings/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever (BookingDeleted id)
        , timeout = Nothing
        , tracker = Nothing
        }


confirmBooking logInfo commentReply bookingInfo =
    let
        bookingVal =
            encodeBookingInfo { bookingInfo | confirmed = True }

        ( subject, body ) =
            confirmationMail bookingInfo commentReply
    in
    secureRequest logInfo
        { method = "PUT"
        , headers = []
        , url =
            "api/restricted/bookings/"
        , body =
            Encode.object
                [ ( "booking", bookingVal )
                , ( "reply_address"
                  , bookingInfo.email
                        |> Encode.string
                  )
                , ( "confirmation_email"
                  , Encode.object
                        [ ( "subject", Encode.string subject )
                        , ( "body", Encode.string body )
                        ]
                  )
                ]
                |> Http.jsonBody
        , expect =
            Http.expectWhatever (BookingConfirmed bookingInfo.bookingId)
        , timeout = Nothing
        , tracker = Nothing
        }


setAvailability logInfo d av =
    let
        ( method, url ) =
            if av == DP.Available then
                ( "DELETE"
                , "api/restricted/availabilities/"
                    ++ String.fromInt (Date.toRataDie d)
                )

            else
                ( "PUT", "api/restricted/availabilities/" )
    in
    secureRequest logInfo
        { method = method
        , headers = []
        , url = url
        , body =
            Encode.object
                [ ( "availability", encodeAvailability d av ) ]
                |> Http.jsonBody
        , expect =
            Http.expectWhatever (AvailabilitySet d)
        , timeout = Nothing
        , tracker = Nothing
        }


encodeAvailability d av =
    Encode.object
        [ ( "date", Encode.int (Date.toRataDie d) )
        , ( "availability"
          , case av of
                DP.Available ->
                    Encode.string "Available"

                DP.NotAvailable ->
                    Encode.string "NotAvailable"

                DP.NoCheckIn ->
                    Encode.string "NoCheckIn"

                DP.NoCheckOut ->
                    Encode.string "NoCheckOut"

                DP.NotAvailableAdmin ->
                    Encode.string "NotAvailable"

                DP.NoCheckInAdmin ->
                    Encode.string "NoCheckIn"

                DP.NoCheckOutAdmin ->
                    Encode.string "NoCheckOut"

                DP.Booked ->
                    Encode.string "Booked"

                DP.BookedAdmin _ ->
                    Encode.string "Booked"

                DP.Locked _ ->
                    Encode.string "Locked"
          )
        ]



-------------------------------------------------------------------------------
------------
-- Emails --
------------


confirmationMail bookingInfo commentReply =
    case bookingInfo.language of
        English ->
            ( "", "" )

        French ->
            ( "Confirmation réservation gite Vieux lilas"
            , "Bonjour "
                ++ bookingInfo.firstName
                ++ " "
                ++ bookingInfo.lastName
                ++ "\n\n"
                ++ "Votre réservation du "
                ++ formatDate bookingInfo.language bookingInfo.checkIn
                ++ " au "
                ++ formatDate bookingInfo.language bookingInfo.checkOut
                ++ " est confirmée.\n\n"
                ++ (case commentReply of
                        Just comment ->
                            comment ++ "\n\n"

                        Nothing ->
                            ""
                   )
                ++ "N'hésitez pas à nous contacter pour toute question.\n\n"
                ++ "Merci de votre confiance et à bientôt!"
            )
