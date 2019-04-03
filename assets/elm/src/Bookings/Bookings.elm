port module Bookings.Bookings exposing (..)

import Bookings.BookingsShared exposing (..)
import Bookings.DatePicker.Date exposing (formatDate)
import Bookings.DatePicker.DatePicker as DP
import Date exposing (..)
import Delay
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
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Http exposing (..)
import Internals.DropdownSelect as Select exposing (..)
import Internals.Helpers exposing (Status(..))
import Internals.PhoenixPresence as Presence exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Value as JsonValue
import List.Extra exposing (uniqueBy)
import MultLang.MultLang exposing (..)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Style.Helpers exposing (..)
import Style.Palette exposing (..)
import Time exposing (..)
import Url


port loadCaptcha : String -> Cmd msg


port captcha_port : (String -> msg) -> Sub msg


port joinChannel : Encode.Value -> Cmd msg


port broadcastLockedDays : Encode.Value -> Cmd msg


port broadcastRefreshAv : (Encode.Value -> msg) -> Sub msg


port receiveInitialLockedDays : (Encode.Value -> msg) -> Sub msg


port receiveLockedDays : (Encode.Value -> msg) -> Sub msg


port presenceState : (Encode.Value -> msg) -> Sub msg


port presenceDiff : (Encode.Value -> msg) -> Sub msg


subscriptions : Model msg -> Sub msg
subscriptions model =
    Sub.map model.outMsg <|
        Sub.batch
            [ captcha_port CaptchaResponse
            , receiveInitialLockedDays ReceiveInitialLockedDays
            , receiveLockedDays ReceiveLockedDays
            , broadcastRefreshAv (\_ -> GetAvailabilities)
            , presenceState ReceivePresenceState
            , presenceDiff ReceivePresenceDiff
            ]


type alias Model msg =
    { checkInPicker : DP.Model Msg
    , checkInDate : Maybe Date
    , checkOutPicker : DP.Model Msg
    , checkOutDate : Maybe Date
    , slots :
        Slots

    --
    , selectedTitle : Maybe Title
    , titleSelector : Select.Model
    , firstName : Maybe String
    , lastName : Maybe String
    , address : Maybe String
    , addAddress : Maybe String
    , postcode : Maybe Int
    , city : Maybe String
    , country : Maybe String
    , phone1 : Maybe String
    , phone2 : Maybe String
    , email : Maybe String
    , confEmail : Maybe String
    , confEmailFocused : Bool
    , nbrAdults : Maybe Int
    , nbrAdultSelector : Select.Model
    , nbrChildren : Maybe Int
    , nbrChildrenSelector : Select.Model
    , comments : Maybe String
    , options : BookingOptions

    --
    , currentSeed : Seed
    , currentUuid : Uuid.Uuid
    , presences : Presence.PresenceState String
    , captchaResp : String
    , bookingProcessed : Status
    , outMsg : Msg -> msg
    }


type DisplayMode
    = DateChoice
    | Form
    | Confirmation


type alias Slots =
    { booked : List Date
    , notAvailable : List Date
    , noCheckIn : List Date
    , noCheckOut : List Date
    , lockedDays : Dict String (List Date)
    }


type Msg
    = CheckInPickerMsg DP.Msg
    | CheckOutPickerMsg DP.Msg
    | SelectTitle Title
    | TitleSelectorMsg Select.Msg
    | SetFirstName String
    | SetLastName String
    | SetAddress String
    | SetAddAddress String
    | SetPostCode String
    | SetCity String
    | SetCountry String
    | SetPhone1 String
    | SetPhone2 String
    | SetEmail String
    | SetConfEmail String
    | SelectNbrAdults Int
    | NbrAdultSelectorMsg Select.Msg
    | SelectNbrChildren Int
    | NbrChildrenSelectorMsg Select.Msg
    | SetComment String
    | SetOption String Bool
    | LoadCaptcha
    | CaptchaResponse String
    | SendBookingData
    | BookingProcessed (Result Http.Error Bool)
    | GetAvailabilities
    | ReceiveAvailabilities (Result Http.Error Slots)
    | ReceiveInitialLockedDays Encode.Value
    | ReceiveLockedDays Encode.Value
    | ReceivePresenceState Decode.Value
    | ReceivePresenceDiff Decode.Value
    | Delay Float Msg
    | NoOp


init outMsg ( seed, seedExtension ) =
    let
        ( checkInPicker, checkInPickerCmd ) =
            DP.init Nothing False CheckInPickerMsg

        ( checkOutPicker, checkOutPickerCmd ) =
            DP.init Nothing False CheckOutPickerMsg

        ( newUuid, newSeed ) =
            step Uuid.generator (initialSeed seed seedExtension)

        slots =
            Slots [] [] [] [] Dict.empty
    in
    ( { checkInPicker = checkInPicker
      , checkInDate =
            Nothing

      --Just <| Date.fromCalendarDate 2020 Time.Jan 2
      , checkOutPicker = checkOutPicker
      , checkOutDate =
            Nothing

      --Just <| Date.fromCalendarDate 2020 Time.Jan 12
      , slots = slots

      --
      --, selectedTitle = Nothing
      --, titleSelector = Select.init
      --, firstName = Nothing
      --, lastName = Nothing
      --, address = Nothing
      --, addAddress = Nothing
      --, postcode = Nothing
      --, city = Nothing
      --, country = Nothing
      --, phone1 = Nothing
      --, phone2 = Nothing
      --, email = Nothing
      --, confEmail = Nothing
      --, confEmailFocused = False
      --, nbrAdults = Nothing
      --, nbrAdultSelector = Select.init
      --, nbrChildren = Nothing
      --, nbrChildrenSelector = Select.init
      --, comments = Nothing
      , selectedTitle = Just Mr
      , titleSelector = Select.init
      , firstName = Just "Florian"
      , lastName = Just "Gillard"
      , address = Just "5 place de l'église"
      , addAddress = Nothing
      , postcode = Just 89520
      , city = Just "Lainsecq"
      , country = Just "France"
      , phone1 = Just "0652110572"
      , phone2 = Nothing
      , email = Just "florian.gillard@tutanota.com"
      , confEmail = Just "florian.gillard@tutanota.com"
      , confEmailFocused = False
      , nbrAdults = Just 1
      , nbrAdultSelector = Select.init
      , nbrChildren = Nothing
      , nbrChildrenSelector = Select.init
      , comments = Nothing
      , options = Dict.empty
      , currentSeed = newSeed
      , currentUuid = newUuid
      , presences = Dict.empty
      , captchaResp = ""
      , bookingProcessed = Initial
      , outMsg = outMsg
      }
    , Cmd.map outMsg <|
        Cmd.batch
            [ checkInPickerCmd
            , checkOutPickerCmd
            , getAvailabilities slots
            , joinChannel (Uuid.encode newUuid)
            ]
    )


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        CheckInPickerMsg pickerMsg ->
            let
                ( newCheckInPicker, cmd, mbDate ) =
                    DP.update pickerMsg model.checkInPicker
            in
            case mbDate of
                Nothing ->
                    ( { model
                        | checkInPicker = newCheckInPicker
                      }
                    , Cmd.map model.outMsg cmd
                    )

                Just checkIn ->
                    ( { model
                        | checkInPicker = newCheckInPicker
                        , checkInDate = mbDate
                        , checkOutPicker =
                            DP.setCurrentDate checkIn model.checkOutPicker
                        , slots = computeBooked model.slots (Just checkIn) model.checkOutDate
                      }
                    , Cmd.batch
                        [ Cmd.map model.outMsg cmd
                        , broadcastLockedDaysCmd (Just checkIn) model.checkOutDate
                        ]
                    )

        CheckOutPickerMsg pickerMsg ->
            let
                ( newCheckOutPicker, cmd, mbDate ) =
                    DP.update pickerMsg model.checkOutPicker
            in
            case mbDate of
                Nothing ->
                    ( { model
                        | checkOutPicker = newCheckOutPicker
                      }
                    , Cmd.map model.outMsg cmd
                    )

                Just checkOut ->
                    ( { model
                        | checkOutPicker = newCheckOutPicker
                        , checkOutDate = mbDate
                        , checkInPicker =
                            DP.setCurrentDate checkOut model.checkInPicker
                        , slots = computeBooked model.slots model.checkInDate (Just checkOut)
                      }
                    , Cmd.batch
                        [ Cmd.map model.outMsg cmd
                        , broadcastLockedDaysCmd model.checkInDate (Just checkOut)
                        ]
                    )

        TitleSelectorMsg selMsg ->
            ( { model
                | titleSelector =
                    Select.update selMsg model.titleSelector
              }
            , Cmd.none
            )

        SelectTitle t ->
            ( { model | selectedTitle = Just t }
            , Cmd.none
            )

        SetFirstName s ->
            ( { model
                | firstName =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetLastName s ->
            ( { model
                | lastName =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetAddress s ->
            ( { model
                | address =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetAddAddress s ->
            ( { model
                | addAddress =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetPostCode s ->
            ( { model
                | postcode =
                    if s == "" then
                        Nothing
                    else
                        String.toInt s
              }
            , Cmd.none
            )

        SetCity s ->
            ( { model
                | city =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetCountry s ->
            ( { model
                | country =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetPhone1 s ->
            ( { model
                | phone1 =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetPhone2 s ->
            ( { model
                | phone2 =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetEmail s ->
            ( { model
                | email =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetConfEmail s ->
            ( { model
                | confEmail =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SelectNbrAdults n ->
            ( { model | nbrAdults = Just n }
            , Cmd.none
            )

        NbrAdultSelectorMsg selMsg ->
            ( { model
                | nbrAdultSelector =
                    Select.update selMsg model.nbrAdultSelector
              }
            , Cmd.none
            )

        SelectNbrChildren n ->
            ( { model | nbrChildren = Just n }
            , Cmd.none
            )

        NbrChildrenSelectorMsg selMsg ->
            ( { model
                | nbrChildrenSelector =
                    Select.update selMsg model.nbrChildrenSelector
              }
            , Cmd.none
            )

        SetComment s ->
            ( { model
                | comments =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        SetOption key b ->
            let
                options =
                    Dict.update key
                        (\mbO ->
                            case mbO of
                                Nothing ->
                                    Nothing

                                Just o ->
                                    Just { o | picked = b }
                        )
                        model.options
            in
            ( { model
                | options = options
              }
            , Cmd.none
            )

        LoadCaptcha ->
            ( model
            , Cmd.map model.outMsg <| loadCaptcha "6Lf9cjgUAAAAAPL3Nu0Z7dHXSbBOW7rEuFLgVsDy"
            )

        CaptchaResponse s ->
            ( { model | captchaResp = s }, Cmd.none )

        SendBookingData ->
            ( { model | bookingProcessed = Waiting }
            , sendBookingData model
                |> Cmd.map model.outMsg
            )

        BookingProcessed res ->
            case res of
                Ok True ->
                    ( { model | bookingProcessed = Success }
                    , Cmd.none
                    )

                _ ->
                    ( { model | bookingProcessed = Failure }
                    , Cmd.none
                    )

        GetAvailabilities ->
            ( model
            , getAvailabilities model.slots
                |> Cmd.map model.outMsg
            )

        ReceiveAvailabilities res ->
            case res of
                Ok slots ->
                    ( { model | slots = slots }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceiveInitialLockedDays json ->
            case Decode.decodeValue (Decode.field "payload" <| Decode.list lockedDaysDecoder) json of
                Ok lDays ->
                    let
                        slots =
                            model.slots

                        newSlots =
                            { slots
                                | lockedDays =
                                    List.foldr
                                        (\{ cIn, cOut, uuid } acc ->
                                            Dict.insert uuid
                                                (daysBooked cIn cOut)
                                                acc
                                        )
                                        slots.lockedDays
                                        lDays
                                        |> Dict.remove (Uuid.toString model.currentUuid)
                            }
                    in
                    ( { model | slots = newSlots }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceiveLockedDays json ->
            case Decode.decodeValue lockedDaysDecoder json of
                Ok { cIn, cOut, uuid } ->
                    if uuid == Uuid.toString model.currentUuid then
                        ( model, Cmd.none )
                    else
                        let
                            slots =
                                model.slots

                            newSlots =
                                { slots
                                    | lockedDays =
                                        Dict.insert uuid
                                            (daysBooked cIn cOut)
                                            slots.lockedDays
                                }

                            checkIn =
                                if
                                    Maybe.map
                                        (checkInAvailability newSlots model.checkOutDate)
                                        model.checkInDate
                                        == Just DP.NotAvailable
                                then
                                    Nothing
                                else
                                    model.checkInDate

                            checkOut =
                                if
                                    Maybe.map
                                        (checkOutAvailability newSlots model.checkInDate)
                                        model.checkOutDate
                                        == Just DP.NotAvailable
                                then
                                    Nothing
                                else
                                    model.checkOutDate
                        in
                        ( { model
                            | slots = newSlots
                            , checkInDate = checkIn
                            , checkOutDate = checkOut
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

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
                        , slots = filterSlots ps model.slots
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
                        , slots = filterSlots ps model.slots
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

        Delay n msg_ ->
            ( model
            , Delay.after n Delay.Millisecond msg_
                |> Cmd.map model.outMsg
            )

        NoOp ->
            ( model, Cmd.none )


filterSlots ps slots =
    { slots
        | lockedDays =
            Dict.filter
                (\uuid _ -> List.member uuid (Dict.keys ps))
                slots.lockedDays
    }


computeBooked slots cIn cOut =
    { slots
        | booked =
            Maybe.map2 daysBooked
                cIn
                cOut
                |> Maybe.withDefault []
    }


checkInAvailability : Slots -> Maybe Date -> (Date -> DP.Availability)
checkInAvailability { booked, notAvailable, noCheckIn, noCheckOut, lockedDays } mbCheckOutDate =
    let
        locked =
            Dict.values lockedDays
                |> List.concat
                |> List.Extra.uniqueBy Date.toRataDie

        isBridging d =
            case mbCheckOutDate of
                Nothing ->
                    False

                Just cOut ->
                    notAvailable
                        |> List.any (\d_ -> Date.compare d_ cOut == LT && Date.compare d_ d == GT)
    in
    \d ->
        if List.member d booked then
            DP.Booked
        else if
            (Maybe.map
                (\cOut ->
                    (Date.compare d (Date.add Days -1 cOut) == GT)
                        || (Date.compare d (Date.add Days -1 cOut) == EQ)
                )
                mbCheckOutDate
                |> Maybe.withDefault False
            )
                || List.member d notAvailable
                || isBridging d
                || List.member d locked
        then
            DP.NotAvailable
        else if List.member d noCheckIn then
            DP.NoCheckIn
        else
            DP.Available


checkOutAvailability : Slots -> Maybe Date -> (Date -> DP.Availability)
checkOutAvailability { booked, notAvailable, noCheckIn, noCheckOut, lockedDays } mbCheckInDate =
    let
        locked =
            Dict.values lockedDays
                |> List.concat
                |> List.Extra.uniqueBy Date.toRataDie

        isBridging d =
            case mbCheckInDate of
                Nothing ->
                    False

                Just cIn ->
                    notAvailable
                        |> List.any (\d_ -> Date.compare d_ cIn == GT && Date.compare d_ d == LT)
    in
    \d ->
        if List.member d booked then
            DP.Booked
        else if
            (Maybe.map
                (\cIn ->
                    (Date.compare d (Date.add Days 1 cIn) == LT)
                        || (Date.compare d (Date.add Days 1 cIn) == EQ)
                )
                mbCheckInDate
                |> Maybe.withDefault False
            )
                || List.member d notAvailable
                || isBridging d
                || List.member d locked
        then
            DP.NotAvailable
        else if List.member d noCheckOut then
            DP.NoCheckOut
        else
            DP.Available


type alias ViewConfig =
    { lang : Lang
    , url : Url.Url
    , width : Int
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ spacing 15
            , padding 15
            , width (maximum 1000 fill)
            , centerX
            , Font.size 16
            ]
            (case String.split "/" config.url.path of
                "" :: "bookings" :: [] ->
                    [ dateChoiceView config model ]

                "" :: "bookings" :: "form" :: [] ->
                    [ formView config model ]

                "" :: "bookings" :: "confirmation" :: [] ->
                    [ confirmView config model ]

                _ ->
                    []
            )



-------------------------------------------------------------------------------


dateChoiceView : ViewConfig -> Model msg -> Element Msg
dateChoiceView config model =
    column
        [ width fill
        , spacing 20
        , height (minimum 400 fill)
        , Background.color (rgba 1 1 1 0.7)
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        ]
        [ el
            [ Font.bold
            , Font.size 22
            , Font.family
                [ Font.typeface "Montserrat"
                , Font.sansSerif
                ]
            ]
            (textM config.lang
                (MultLangStr "Online booking"
                    "Réservation en ligne"
                )
            )
        , column [ spacing 15 ]
            [ paragraph
                []
                [ textM config.lang
                    (MultLangStr
                        "This page allows you to quickly book your stay online. "
                        "Cette page vous permet de réserver rapidement votre séjour en ligne. "
                    )
                ]
            , paragraph
                []
                [ textM config.lang
                    (MultLangStr
                        "You can check that the gîte is available at the desired time using the calendar below."
                        "Vous pouvez verifier que le gîte est disponible à la période souhaitée à l'aide des calendriers ci-dessous."
                    )
                ]
            , paragraph
                []
                [ textM config.lang
                    (MultLangStr
                        "The minimum stay is two nights."
                        "La durée minimum du séjour est de deux nuits."
                    )
                ]
            ]
        , (if config.width < 1000 then
            column
           else
            row
          )
            [ spacing 15
            , width fill
            ]
            [ checkInView config model
            , checkOutView config model
            , if model.options == Dict.empty then
                Element.none
              else
                optionsView config model
            ]
        , el
            [ alignLeft
            , alignBottom
            ]
            (link
                (buttonStyle2 (canShowForm model))
                { url =
                    if canShowForm model then
                        "/bookings/form"
                    else
                        ""
                , label =
                    textM config.lang
                        (MultLangStr "Next" "Suivant")
                }
            )
        ]


optionsView config model =
    let
        optionView { name, key, price, picked } =
            Input.checkbox
                []
                { onChange = SetOption key
                , icon = Input.defaultCheckbox
                , checked = picked
                , label =
                    Input.labelRight
                        []
                        (el
                            []
                            (textM config.lang name)
                        )
                }
    in
    column
        [ spacing 15 ]
        [ el
            [ Font.bold
            , Font.size 18
            , Font.family
                [ Font.typeface "Montserrat"
                , Font.sansSerif
                ]
            ]
            (textM config.lang
                (MultLangStr "Options"
                    "Options"
                )
            )
        , column
            [ spacing 15 ]
            (List.map optionView (Dict.values model.options))
        ]


canShowForm model =
    case ( model.checkInDate, model.checkOutDate ) of
        ( Just _, Just _ ) ->
            True

        _ ->
            False


checkInView : { a | lang : Lang } -> Model msg -> Element Msg
checkInView config model =
    column
        [ spacing 15
        , width fill
        , Background.color lightGrey
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        , alignTop
        ]
        [ el [ Font.bold ]
            (textM config.lang
                (MultLangStr "Check-In"
                    "Date d'arrivée"
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
            , if model.slots.noCheckIn == [] then
                Element.none
              else
                row
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
                            (MultLangStr "Available but not as checkin date"
                                "Libre mais pas comme date d'arrivée"
                            )
                        )
                    ]
            ]
        , DP.view
            { lang = config.lang
            , availability = checkInAvailability model.slots model.checkOutDate
            , pickedDate = model.checkInDate
            }
            model.checkInPicker
        ]


checkOutView : { a | lang : Lang } -> Model msg -> Element Msg
checkOutView config model =
    column
        [ spacing 15
        , width fill
        , Background.color lightGrey
        , padding 15
        , Border.rounded 5
        , Border.color grey
        , Border.width 1
        , alignTop
        ]
        [ el [ Font.bold ]
            (textM config.lang
                (MultLangStr "Check-out"
                    "Date de départ"
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
            , if model.slots.noCheckOut == [] then
                Element.none
              else
                row
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
                            (MultLangStr
                                "Available but not as checkout date"
                                "Libre mais pas comme date de départ"
                            )
                        )
                    ]
            ]
        , DP.view
            { lang = config.lang
            , availability = checkOutAvailability model.slots model.checkInDate
            , pickedDate = model.checkOutDate
            }
            model.checkOutPicker
        ]



-------------------------------------------------------------------------------


formView : ViewConfig -> Model msg -> Element Msg
formView config model =
    case ( model.checkInDate, model.checkOutDate ) of
        ( Just cInDate, Just cOutDate ) ->
            column
                [ padding 10
                , spacing 10
                , height (minimum 400 fill)
                , width fill
                , Background.color (rgba 1 1 1 0.7)
                , padding 15
                , Border.rounded 5
                , Border.color grey
                , Border.width 1
                ]
                [ el
                    [ Font.bold
                    , Font.size 22
                    , Font.family
                        [ Font.typeface "Montserrat"
                        , Font.sansSerif
                        ]
                    ]
                    (textM config.lang
                        (MultLangStr "Guest information"
                            "Informations client"
                        )
                    )
                , row
                    [ spacing 20 ]
                    [ el
                        []
                        (text <|
                            strM config.lang
                                (MultLangStr
                                    "Check-In"
                                    "Date d'arrivée"
                                )
                                ++ " : "
                                ++ formatDate config.lang cInDate
                        )
                    , el
                        []
                        (text <|
                            strM config.lang
                                (MultLangStr
                                    "Check-out"
                                    "Date de départ"
                                )
                                ++ " : "
                                ++ formatDate config.lang cOutDate
                        )
                    ]
                , Select.view
                    { outMsg = TitleSelectorMsg
                    , items =
                        [ ( strM config.lang
                                (MultLangStr
                                    "Mr"
                                    "M."
                                )
                          , SelectTitle Mr
                          )
                        , ( strM config.lang
                                (MultLangStr
                                    "Ms"
                                    "Mme"
                                )
                          , SelectTitle Ms
                          )
                        , ( strM config.lang
                                (MultLangStr
                                    "Other"
                                    "Autre"
                                )
                          , SelectTitle Other
                          )
                        ]
                    , selected =
                        model.selectedTitle
                            |> Maybe.map
                                (\t -> strM config.lang (titleMLS t))
                    , placeholder =
                        Just "-"
                    , label =
                        Just <|
                            mandatoryLabel config
                                (MultLangStr
                                    "Title"
                                    "Civilité"
                                )
                    }
                    model.titleSelector
                , Input.text
                    textInputStyle_
                    { onChange = SetFirstName
                    , text =
                        model.firstName
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "First Name"
                                "Prénom"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetLastName
                    , text =
                        model.lastName
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Last Name"
                                "Nom"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetAddress
                    , text =
                        model.address
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Address"
                                "Adresse"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetAddAddress
                    , text =
                        model.addAddress
                            |> Maybe.withDefault ""
                    , label =
                        regLabel config
                            (MultLangStr "Additional address"
                                "Complément d'adresse"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetPostCode
                    , text =
                        model.postcode
                            |> Maybe.map String.fromInt
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Post Code"
                                "Code postal"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetCity
                    , text =
                        model.city
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "City"
                                "Ville"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetCountry
                    , text =
                        model.country
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Country"
                                "Pays"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetPhone1
                    , text =
                        model.phone1
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Phone number 1"
                                "Téléphone 1"
                            )
                    , placeholder = Nothing
                    }
                , Input.text
                    textInputStyle_
                    { onChange = SetPhone2
                    , text =
                        model.phone2
                            |> Maybe.withDefault ""
                    , label =
                        regLabel config
                            (MultLangStr "Phone number 2"
                                "Téléphone 2"
                            )
                    , placeholder = Nothing
                    }
                , Input.email
                    textInputStyle_
                    { onChange = SetEmail
                    , text =
                        model.email
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Email"
                                "Email"
                            )
                    , placeholder = Nothing
                    }
                , Input.email
                    textInputStyle_
                    { onChange = SetConfEmail
                    , text =
                        model.confEmail
                            |> Maybe.withDefault ""
                    , label =
                        mandatoryLabel config
                            (MultLangStr "Confirm Email"
                                "Confirmation Email"
                            )
                    , placeholder = Nothing
                    }
                , Select.view
                    { outMsg = NbrAdultSelectorMsg
                    , items =
                        [ ( "1", SelectNbrAdults 1 )
                        , ( "2", SelectNbrAdults 2 )
                        , ( "3", SelectNbrAdults 3 )
                        , ( "4", SelectNbrAdults 4 )
                        , ( "5", SelectNbrAdults 5 )
                        , ( "6", SelectNbrAdults 6 )
                        ]
                    , selected =
                        model.nbrAdults
                            |> Maybe.map String.fromInt
                    , placeholder =
                        Just "-"
                    , label =
                        Just <|
                            mandatoryLabel config
                                (MultLangStr
                                    "Number of adults"
                                    "Nombre d'adultes"
                                )
                    }
                    model.nbrAdultSelector
                , Select.view
                    { outMsg = NbrChildrenSelectorMsg
                    , items =
                        [ ( "1", SelectNbrChildren 1 )
                        , ( "2", SelectNbrChildren 2 )
                        , ( "3", SelectNbrChildren 3 )
                        , ( "4", SelectNbrChildren 4 )
                        , ( "5", SelectNbrChildren 5 )
                        , ( "6", SelectNbrChildren 6 )
                        ]
                    , selected =
                        model.nbrChildren
                            |> Maybe.map String.fromInt
                    , placeholder =
                        Just "-"
                    , label =
                        Just <|
                            mandatoryLabel config
                                (MultLangStr
                                    "Number of children"
                                    "Nombre d'enfants"
                                )
                    }
                    model.nbrChildrenSelector
                , Input.multiline
                    [ height (px 100)
                    , width (px 400)
                    ]
                    { onChange = SetComment
                    , text =
                        model.comments
                            |> Maybe.withDefault ""
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove
                            [ paddingXY 0 10 ]
                            (textM config.lang
                                (MultLangStr "Remarks / Requests"
                                    "Remarques / Demandes particulières"
                                )
                            )
                    , spellcheck = False
                    }
                , row
                    [ spacing 15 ]
                    [ link
                        (buttonStyle2 True)
                        { url = "/bookings"
                        , label =
                            textM config.lang
                                (MultLangStr "Go back" "Retour")
                        }
                    , link
                        (buttonStyle2 True)
                        { url = "/bookings/confirmation"
                        , label =
                            textM config.lang
                                (MultLangStr "Next" "Suivant")
                        }
                    ]
                ]

        _ ->
            column
                []
                [ link
                    (buttonStyle2 True)
                    { url = "/bookings"
                    , label =
                        textM config.lang
                            (MultLangStr "Go back" "Retour")
                    }
                ]



-------------------------------------------------------------------------------


confirmView : ViewConfig -> Model msg -> Element Msg
confirmView config model =
    case ( ( model.checkInDate, model.checkOutDate ), ( validateForm model, toBookingInfo model ) ) of
        ( ( Just cInDate, Just cOutDate ), ( True, Ok bookingInfo ) ) ->
            let
                textS mbStr =
                    text <| Maybe.withDefault "" mbStr

                nc =
                    nightsCount cInDate cOutDate
            in
            column
                [ spacing 20
                , width fill
                , height (minimum 400 fill)
                , Background.color (rgba 1 1 1 0.7)
                , padding 15
                , Border.rounded 5
                , Border.color grey
                , Border.width 1
                ]
                [ el
                    [ Font.bold
                    , Font.size 22
                    , Font.family
                        [ Font.typeface "Montserrat"
                        , Font.sansSerif
                        ]
                    ]
                    (textM config.lang
                        (MultLangStr "Confirmation"
                            "Confirmation"
                        )
                    )
                , customerDetailView config bookingInfo
                , contactView config bookingInfo
                , recapView config cInDate cOutDate bookingInfo
                , html <|
                    Html.img
                        [ HtmlAttr.hidden True
                        , HtmlEvents.on "load"
                            (Decode.succeed (Delay 200 LoadCaptcha))
                        , HtmlAttr.src "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"
                        ]
                        []
                , el
                    [ htmlAttribute <|
                        HtmlAttr.class "g-recaptcha"
                    , paddingEach
                        { sides | top = 10 }
                    ]
                    Element.none
                , row
                    [ spacing 15 ]
                    [ link
                        (buttonStyle2 True)
                        { url = "/bookings/form"
                        , label =
                            textM config.lang
                                (MultLangStr "Go back" "Retour")
                        }
                    , case model.bookingProcessed of
                        Initial ->
                            Input.button
                                (buttonStyle_ (model.captchaResp /= ""))
                                { onPress =
                                    if model.captchaResp /= "" then
                                        Just SendBookingData
                                    else
                                        Nothing
                                , label =
                                    textM config.lang
                                        (MultLangStr "Send"
                                            "Envoyer"
                                        )
                                }

                        Waiting ->
                            el
                                []
                                (textM config.lang
                                    (MultLangStr "Your request is being processed, please wait... "
                                        "Votre demande est en cours de traitement, veuillez patienter... "
                                    )
                                )

                        Success ->
                            el
                                []
                                (textM config.lang
                                    (MultLangStr "Your request has been processed, you will receive a confirmation email in the next 24H."
                                        "Votre demande à été prise en compte, vous allez recevoir un email de confirmation dans les prochaines 24H."
                                    )
                                )

                        Failure ->
                            el
                                []
                                (text <| "Failure")
                    ]
                ]

        _ ->
            column
                []
                [ link
                    (buttonStyle2 True)
                    { url = "/bookings"
                    , label =
                        textM config.lang
                            (MultLangStr "Go back" "Retour")
                    }
                ]



-------------------------------------------------------------------------------
------------------
-- Http && Json --
------------------


sendBookingData : Model msg -> Cmd Msg
sendBookingData model =
    let
        body =
            Encode.object
                [ ( "booking"
                  , encodeBookingData model
                  )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/api/bookings"
        , body = body
        , expect = Http.expectJson BookingProcessed decodeBookingResult
        }


decodeBookingResult =
    Decode.field "message" (Decode.succeed "success")
        |> Decode.map (\s -> s == "success")


encodeBookingData : Model msg -> Encode.Value
encodeBookingData model =
    let
        strEncode s =
            Maybe.map Encode.string s
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "check_in"
          , model.checkInDate
                |> Maybe.map Date.toRataDie
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "check_out"
          , model.checkOutDate
                |> Maybe.map Date.toRataDie
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "title"
          , model.selectedTitle
                |> Maybe.map encodeTitle
                |> Maybe.withDefault Encode.null
          )
        , ( "first_name"
          , strEncode model.firstName
          )
        , ( "last_name"
          , strEncode model.lastName
          )
        , ( "address"
          , strEncode model.address
          )
        , ( "add_address"
          , strEncode model.addAddress
          )
        , ( "postcode"
          , model.postcode
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "city"
          , strEncode model.city
          )
        , ( "country"
          , strEncode model.country
          )
        , ( "phone1"
          , strEncode model.phone1
          )
        , ( "phone2"
          , strEncode model.phone2
          )
        , ( "email"
          , strEncode model.email
          )
        , ( "nbr_adults"
          , model.nbrAdults
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "nbr_children"
          , model.nbrChildren
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "comments"
          , strEncode model.comments
          )
        , ( "options"
          , encodeBookingOptions
                model.options
          )
        , ( "days_booked"
          , model.slots.booked
                |> List.map Date.toRataDie
                |> Encode.list
                    (\d ->
                        Encode.object
                            [ ( "date", Encode.int d )
                            , ( "availability", Encode.string "Booked" )
                            ]
                    )
          )
        , ( "captcha_response", Encode.string model.captchaResp )
        ]


broadcastLockedDaysCmd : Maybe Date -> Maybe Date -> Cmd msg
broadcastLockedDaysCmd mbCIn mbCOut =
    case ( mbCIn, mbCOut ) of
        ( Just cIn, Just cOut ) ->
            broadcastLockedDays <|
                Encode.object
                    [ ( "cIn", Encode.int <| Date.toRataDie cIn )
                    , ( "cOut", Encode.int <| Date.toRataDie cOut )
                    ]

        _ ->
            Cmd.none


getAvailabilities slots =
    Http.get
        { url = "/api/availabilities"
        , expect =
            Http.expectJson
                ReceiveAvailabilities
                (decodeSlots slots)
        }


decodeSlots currentSlots =
    let
        currentSlots_ =
            { currentSlots | notAvailable = [], noCheckIn = [], noCheckOut = [] }

        swapBooked =
            List.map
                (\( d, av ) ->
                    if av == DP.Booked then
                        ( d, DP.NotAvailable )
                    else
                        ( d, av )
                )

        putInSlot ( d, av ) ({ booked, notAvailable, noCheckIn, noCheckOut } as slots) =
            case av of
                DP.NotAvailable ->
                    { slots | notAvailable = d :: slots.notAvailable }

                DP.NoCheckIn ->
                    { slots | noCheckIn = d :: slots.noCheckIn }

                DP.NoCheckOut ->
                    { slots | noCheckOut = d :: slots.noCheckOut }

                _ ->
                    slots
    in
    Decode.field "data"
        (Decode.list
            (Decode.map2 Tuple.pair
                (Decode.field "date" (Decode.map Date.fromRataDie Decode.int))
                (Decode.field "availability" decodeAvailability)
            )
            |> Decode.map swapBooked
            |> Decode.map (List.foldr (\x acc -> putInSlot x acc) currentSlots_)
        )


decodeAvailability =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "NotAvailable" ->
                        Decode.succeed DP.NotAvailable

                    "NoCheckIn" ->
                        Decode.succeed DP.NoCheckIn

                    "NoCheckOut" ->
                        Decode.succeed DP.NoCheckOut

                    "Booked" ->
                        Decode.succeed DP.Booked

                    somethingElse ->
                        Decode.fail <|
                            "Unknown CellContent: "
                                ++ somethingElse
            )



-------------------------------------------------------------------------------


toBookingInfo model =
    encodeBookingData model
        |> JsonValue.decodeValue
        |> JsonValue.setIn [ "confirmed" ] (JsonValue.BoolValue False)
        |> Result.withDefault JsonValue.NullValue
        |> JsonValue.setIn [ "id" ] (JsonValue.NumericValue 0)
        |> Result.withDefault JsonValue.NullValue
        |> JsonValue.encode
        |> Decode.decodeValue decodeBookingInfo


validateForm : Model msg -> Bool
validateForm { selectedTitle, firstName, lastName, address, postcode, city, country, phone1, email, confEmail, nbrAdults } =
    let
        validTitle =
            case selectedTitle of
                Nothing ->
                    False

                Just _ ->
                    True

        validFstName =
            firstName /= Nothing

        validLstName =
            lastName /= Nothing

        validAddr =
            address /= Nothing

        validPostcode =
            postcode /= Nothing

        validCity =
            city /= Nothing

        validCountry =
            country /= Nothing

        validPhone1 =
            phone1 /= Nothing

        validEmail =
            (email /= Nothing)
                && (email == confEmail)

        validNbrAdults =
            case nbrAdults of
                Nothing ->
                    False

                Just _ ->
                    True
    in
    validTitle
        && validFstName
        && validLstName
        && validAddr
        && validPostcode
        && validCity
        && validCountry
        && validPhone1
        && validEmail
        && validNbrAdults
