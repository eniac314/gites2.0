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
    , nbrAdults : Maybe Int
    , nbrAdultSelector : Select.Model
    , nbrChildren : Maybe Int
    , nbrChildrenSelector : Select.Model
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
    | SetConfEmail String
    | SelectNbrAdults Int
    | NbrAdultSelectorMsg Select.Msg
    | SelectNbrChildren Int
    | NbrChildrenSelectorMsg Select.Msg
    | SetComment String
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
      , nbrAdults = Nothing
      , nbrAdultSelector = Select.init
      , nbrChildren = Nothing
      , nbrChildrenSelector = Select.init
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
    , width : Int
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

                "" :: "bookings" :: "confirmation" :: [] ->
                    [ confirmView config model ]

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
                        []
                        { url = "/bookings/form"
                        , label =
                            textM config.lang
                                (MultLangStr "Go back" "Retour")
                        }
                    , link
                        []
                        { url = "/bookings/confirmation"
                        , label =
                            textM config.lang
                                (MultLangStr "Next" "Suivant")
                        }
                    ]
                ]

        _ ->
            Element.none


regLabel config mls =
    (if config.width < 800 then
        Input.labelAbove
     else
        Input.labelLeft
    )
        [ centerY
        , width (px 200)
        ]
        (text <| strM config.lang mls)


mandatoryLabel config mls =
    (if config.width < 800 then
        Input.labelAbove
     else
        Input.labelLeft
    )
        [ centerY
        , width (px 200)
        ]
        (row
            [ spacing 2 ]
            [ text <| strM config.lang mls
            , redStar
            ]
        )



-------------------------------------------------------------------------------


confirmView : ViewConfig -> Model msg -> Element Msg
confirmView config model =
    case ( model.checkInDate, model.checkOutDate, validateForm model ) of
        ( Just cInDate, Just cOutDate, True ) ->
            let
                textS mbStr =
                    text <| Maybe.withDefault "" mbStr

                nc =
                    nightsCount cInDate cOutDate
            in
            column
                [ spacing 20 ]
                [ el
                    [ Font.bold
                    , Font.size 22
                    ]
                    (textM config.lang
                        (MultLangStr "Confirmation"
                            "Confirmation"
                        )
                    )
                , column
                    [ spacing 15 ]
                    [ el
                        [ Font.size 20
                        ]
                        (textM config.lang
                            (MultLangStr "Customer details"
                                "Informations client"
                            )
                        )
                    , row
                        [ spacing 5 ]
                        [ textM config.lang <|
                            case Maybe.withDefault Mr model.selectedTitle of
                                Mr ->
                                    MultLangStr "Mr"
                                        "M."

                                Ms ->
                                    MultLangStr "Ms"
                                        "Mme"

                                Other ->
                                    MultLangStr "Other"
                                        "Autre"
                        , el [] (textS model.firstName)
                        , el [] (textS model.lastName)
                        ]
                    , paragraph [] [ textS model.address ]
                    , case model.addAddress of
                        Just addAddress ->
                            paragraph [] [ text addAddress ]

                        _ ->
                            Element.none
                    , el [] (textS (Maybe.map String.fromInt model.postcode))
                    , el [] (textS model.city)
                    , el [] (textS model.country)
                    ]
                , column
                    [ spacing 15 ]
                    [ el
                        [ Font.size 20
                        ]
                        (text "Contact")
                    , el [] (textS model.phone1)
                    , case model.phone2 of
                        Just phone2 ->
                            el [] (text phone2)

                        _ ->
                            Element.none
                    , el [] (textS model.email)
                    ]
                , column
                    [ spacing 15 ]
                    [ el
                        [ Font.size 20
                        ]
                        (textM config.lang
                            (MultLangStr "Your booking"
                                "Votre réservation"
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
                    , case model.nbrAdults of
                        Just n ->
                            el
                                []
                                (text <|
                                    strM config.lang
                                        (MultLangStr
                                            "Number of adults"
                                            "Nombre d'adultes"
                                        )
                                        ++ " : "
                                        ++ String.fromInt n
                                )

                        Nothing ->
                            Element.none
                    , case model.nbrChildren of
                        Just n ->
                            el
                                []
                                (text <|
                                    strM config.lang
                                        (MultLangStr
                                            "Number of children"
                                            "Nombre d'enfants"
                                        )
                                        ++ " : "
                                        ++ String.fromInt n
                                )

                        Nothing ->
                            Element.none
                    , el
                        []
                        (text <|
                            strM config.lang
                                (MultLangStr "Your stay: "
                                    "Réservation pour "
                                )
                                ++ String.fromInt nc
                                ++ (if nc > 1 then
                                        strM config.lang
                                            (MultLangStr " nights"
                                                " nuits"
                                            )
                                    else
                                        strM config.lang
                                            (MultLangStr " night"
                                                " nuit"
                                            )
                                   )
                        )
                    , el
                        []
                        (text <|
                            "Total: "
                                ++ String.fromInt (nc * 50)
                                ++ " €"
                        )
                    ]
                , case model.comments of
                    Just _ ->
                        column
                            [ spacing 15 ]
                            [ el
                                [ Font.size 20
                                ]
                                (textM config.lang
                                    (MultLangStr "Remarks / Requests"
                                        "Remarques / Demandes particulières"
                                    )
                                )
                            , paragraph [] [ textS model.comments ]
                            ]

                    _ ->
                        Element.none
                ]

        _ ->
            Element.none



-------------------------------------------------------------------------------


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


nightsCount : Date -> Date -> Int
nightsCount d1 d2 =
    Date.diff Date.Days d1 d2
