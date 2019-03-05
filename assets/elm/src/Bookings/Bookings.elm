port module Bookings.Bookings exposing (..)

import Bookings.DatePicker.Date exposing (formatDate)
import Bookings.DatePicker.DatePicker as DP
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
import Style.Helpers exposing (..)
import Style.Palette exposing (..)
import Url


type alias Model msg =
    { checkInPicker : DP.Model Msg
    , checkInDate : Maybe Date
    , checkInAvailability : Date -> DP.Availability
    , checkOutPicker : DP.Model Msg
    , checkOutDate : Maybe Date
    , checkOutAvailability : Date -> DP.Availability
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
    , comments : Maybe String

    --
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
            DP.init Nothing CheckInPickerMsg

        ( checkOutPicker, checkOutPickerCmd ) =
            DP.init Nothing CheckOutPickerMsg
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
      , comments = Nothing

      --
      , outMsg = outMsg
      }
    , Cmd.map outMsg <|
        Cmd.batch
            [ checkInPickerCmd
            , checkOutPickerCmd
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
                        , checkOutAvailability =
                            newCheckOutAvailability model.slots checkIn
                      }
                    , Cmd.map model.outMsg cmd
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
                        , checkInAvailability =
                            newCheckInAvailability model.slots checkOut
                      }
                    , Cmd.map model.outMsg cmd
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
                | comments =
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


type alias ViewConfig =
    { lang : Lang
    , url : Url.Url

    --, width : Int
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ spacing 15
            , padding 15
            , width fill
            , Font.size 16
            ]
            (case String.split "/" config.url.path of
                "" :: "bookings" :: [] ->
                    [ dateChoiceView config model ]

                "" :: "bookings" :: "form" :: [] ->
                    [ formView config model ]

                _ ->
                    []
            )



-------------------------------------------------------------------------------


dateChoiceView : ViewConfig -> Model msg -> Element Msg
dateChoiceView config model =
    column
        [ spacing 15
        , width fill
        ]
        [ checkInView config model
        , checkOutView config model
        , link
            []
            { url = "/bookings/form"
            , label =
                textM config.lang
                    (MultLangStr "Next" "Suivant")
            }
        ]


checkInView : { a | lang : Lang } -> Model msg -> Element Msg
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


checkOutView : { a | lang : Lang } -> Model msg -> Element Msg
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



-------------------------------------------------------------------------------


formView : ViewConfig -> Model msg -> Element Msg
formView config model =
    case ( model.checkInDate, model.checkOutDate ) of
        ( Just cInDate, Just cOutDate ) ->
            column
                [ padding 10
                , spacing 10
                ]
                [ row
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
                        [ ( "Mr", SelectTitle Mr )
                        , ( "Ms", SelectTitle Ms )
                        , ( "Other", SelectTitle Other )
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
                , Input.text
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
                , Input.text
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
                ]

        _ ->
            Element.none


regLabel config mls =
    (if False then
        Input.labelAbove
     else
        Input.labelLeft
    )
        [ centerY
        , width (px 170)
        ]
        (text <| strM config.lang mls)


mandatoryLabel config mls =
    (if False then
        Input.labelAbove
     else
        Input.labelLeft
    )
        [ centerY
        , width (px 170)
        ]
        (row
            [ spacing 2 ]
            [ text <| strM config.lang mls
            , redStar
            ]
        )
