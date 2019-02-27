port module Bookings.Bookings exposing (..)

import Bookings.DatePicker.DatePicker as DP
import Browser exposing (element)
import Date exposing (..)
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
import Internals.DropdownSelect as Select exposing (..)
import MultLang.MultLang exposing (..)
import Style.Palette exposing (..)


main : Program () (Model Msg) Msg
main =
    Browser.element
        { init = \_ -> init identity
        , update = update
        , view = view { lang = French }
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch []


type alias Model msg =
    { checkInPicker : DP.Model msg
    , checkInDate : Maybe Date
    , checkInAvailability : Date -> DP.Availability
    , checkOutPicker : DP.Model msg
    , checkOutDate : Maybe Date
    , checkOutAvailability : Date -> DP.Availability
    , slots : Slots

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
    , comment : Maybe String

    --
    , displayMode : DisplayMode
    , outMsg : Msg -> msg
    }


type Title
    = Mr
    | Ms
    | Other


titleMLS t =
    case t of
        Mr ->
            MultLangStr "Mr" "M."

        Ms ->
            MultLangStr "Ms" "Mme"

        Other ->
            MultLangStr "Other" "Autre"


type DisplayMode
    = DateChoice
    | Form
    | Confirmation


type alias Slots =
    { booked : List Date
    , notAvailable : List Date
    , noCheckIn : List Date
    , noCheckOut : List Date
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
    | SetComment String
    | SetConfEmail String
    | NoOp


init outMsg =
    let
        ( checkInPicker, checkInPickerCmd ) =
            DP.init Nothing (outMsg << CheckInPickerMsg)

        ( checkOutPicker, checkOutPickerCmd ) =
            DP.init Nothing (outMsg << CheckOutPickerMsg)
    in
    ( { checkInPicker = checkInPicker
      , checkInDate = Nothing
      , checkInAvailability = always DP.Available
      , checkOutPicker = checkOutPicker
      , checkOutDate = Nothing
      , checkOutAvailability = always DP.Available
      , slots =
            Slots [] [] [] []

      --
      , selectedTitle = Nothing
      , titleSelector = Select.init
      , firstName = Nothing
      , lastName = Nothing
      , address = Nothing
      , addAddress = Nothing
      , postcode = Nothing
      , city = Nothing
      , country = Nothing
      , phone1 = Nothing
      , phone2 = Nothing
      , email = Nothing
      , confEmail = Nothing
      , confEmailFocused = False
      , comment = Nothing

      --
      , displayMode = DateChoice
      , outMsg = outMsg
      }
    , Cmd.batch
        [ checkInPickerCmd
        , checkOutPickerCmd
        ]
    )


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
                    , cmd
                    )

                Just checkIn ->
                    ( { model
                        | checkInPicker = newCheckInPicker
                        , checkInDate = mbDate
                        , checkOutPicker =
                            DP.setCurrentDate checkIn model.checkOutPicker
                        , checkOutAvailability =
                            newCheckOutAvailability model.slots checkIn
                      }
                    , cmd
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
                    , cmd
                    )

                Just checkOut ->
                    ( { model
                        | checkOutPicker = newCheckOutPicker
                        , checkOutDate = mbDate
                        , checkInPicker =
                            DP.setCurrentDate checkOut model.checkInPicker
                        , checkInAvailability =
                            newCheckInAvailability model.slots checkOut
                      }
                    , cmd
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

        SetComment s ->
            ( { model
                | comment =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


newCheckInAvailability : Slots -> Date -> (Date -> DP.Availability)
newCheckInAvailability { booked, notAvailable, noCheckIn, noCheckOut } checkOutDate =
    \d ->
        if
            (Date.compare d checkOutDate == GT)
                || (Date.compare d checkOutDate == EQ)
                || List.member d booked
                || List.member d notAvailable
        then
            DP.NotAvailable
        else if List.member d noCheckOut then
            DP.NoCheckOut
        else
            DP.Available


newCheckOutAvailability : Slots -> Date -> (Date -> DP.Availability)
newCheckOutAvailability { booked, notAvailable, noCheckIn, noCheckOut } checkInDate =
    \d ->
        if
            (Date.compare d checkInDate == LT)
                || (Date.compare d checkInDate == EQ)
                || List.member d booked
                || List.member d notAvailable
        then
            DP.NotAvailable
        else if List.member d noCheckOut then
            DP.NoCheckOut
        else
            DP.Available



--view : { a | lang : Lang } -> Model msg -> Html msg


view config model =
    Element.layout [] <|
        Element.map model.outMsg <|
            column
                [ spacing 15
                , padding 15
                , width fill
                , Font.size 16
                ]
                [ checkInView config model
                , checkOutView config model
                , Select.view
                    { outMsg = TitleSelectorMsg
                    , items =
                        [ ( "Mr", SelectTitle Mr )
                        , ( "Ms", SelectTitle Ms )
                        , ( "Other", SelectTitle Other )
                        ]
                    , selected =
                        model.selectedTitle
                            |> Maybe.map
                                (\t -> strM config.lang (titleMLS t))
                    , placeholder =
                        Just <|
                            strM config.lang
                                (MultLangStr
                                    "Title"
                                    "Civilité"
                                )
                    , label = Nothing
                    }
                    model.titleSelector
                ]


checkInView : { a | lang : Lang } -> Model msg -> Element msg
checkInView config model =
    column
        [ spacing 15
        , width fill
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
                        , Background.color lightGreen
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
                        , Background.color lightRed
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
                [ el
                    [ width (px 15)
                    , height (px 15)
                    , Border.color grey
                    , Border.width 1
                    , Background.color orange
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
            , availability = model.checkInAvailability
            , pickedDate = model.checkInDate
            }
            model.checkInPicker
        ]


checkOutView : { a | lang : Lang } -> Model msg -> Element msg
checkOutView config model =
    column
        [ spacing 15
        , width fill
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
                        , Background.color lightGreen
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
                        , Background.color lightRed
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
                [ el
                    [ width (px 15)
                    , height (px 15)
                    , Border.color grey
                    , Border.width 1
                    , Background.color orange
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
            , availability = model.checkOutAvailability
            , pickedDate = model.checkOutDate
            }
            model.checkOutPicker
        ]
