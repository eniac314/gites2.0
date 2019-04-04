module Bookings.BookingsShared exposing (..)

import Bookings.DatePicker.Date exposing (formatDate)
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


type alias BookingInfo =
    { bookingId : Int
    , checkIn : Date
    , checkOut : Date
    , title : Title
    , firstName : String
    , lastName : String
    , address : String
    , addAddress : Maybe String
    , postcode : Int
    , city : String
    , country : String
    , phone1 : String
    , phone2 : Maybe String
    , email : String
    , nbrAdults : Int
    , nbrKids : Maybe Int
    , comment : Maybe String
    , options : BookingOptions
    , confirmed : Bool
    }


type alias BookingOptions =
    { oneDayPrice : Maybe Float
    , discountPrice : Maybe Float
    , oneWeekPrice : Maybe Float
    , touristTax : Maybe Float
    , options : Dict String BookingOption
    }


dummyOptions =
    { oneDayPrice = Nothing
    , discountPrice = Nothing
    , oneWeekPrice = Nothing
    , touristTax = Nothing
    , options = Dict.empty
    }


type alias BookingOption =
    { name : MultLangStr
    , price : Float
    , key : String
    , picked : Bool
    }


encodeBookingInfo : BookingInfo -> Encode.Value
encodeBookingInfo bookingInfo =
    Encode.object
        [ ( "bookingId"
          , Encode.int bookingInfo.bookingId
          )
        , ( "check_in"
          , bookingInfo.checkIn
                |> Date.toRataDie
                |> Encode.int
          )
        , ( "check_out"
          , bookingInfo.checkOut
                |> Date.toRataDie
                |> Encode.int
          )
        , ( "title"
          , bookingInfo.title
                |> encodeTitle
          )
        , ( "first_name"
          , Encode.string bookingInfo.firstName
          )
        , ( "last_name"
          , Encode.string bookingInfo.lastName
          )
        , ( "address"
          , Encode.string bookingInfo.address
          )
        , ( "add_address"
          , bookingInfo.addAddress
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "postcode"
          , bookingInfo.postcode
                |> Encode.int
          )
        , ( "city"
          , Encode.string bookingInfo.city
          )
        , ( "country"
          , Encode.string bookingInfo.country
          )
        , ( "phone1"
          , Encode.string bookingInfo.phone1
          )
        , ( "phone2"
          , bookingInfo.phone2
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "email"
          , Encode.string bookingInfo.email
          )
        , ( "nbr_adults"
          , bookingInfo.nbrAdults
                |> Encode.int
          )
        , ( "nbr_children"
          , bookingInfo.nbrKids
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "comments"
          , bookingInfo.comment
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "options"
          , encodeBookingOptions bookingInfo.options
          )
        , ( "confirmed"
          , Encode.bool bookingInfo.confirmed
          )
        ]


encodeTitle : Title -> Encode.Value
encodeTitle title =
    case title of
        Mr ->
            Encode.string "Mr"

        Ms ->
            Encode.string "Ms"

        Other ->
            Encode.string "Other"


encodeBookingOptions : BookingOptions -> Encode.Value
encodeBookingOptions bos =
    Encode.object
        [ ( "oneDayPrice"
          , bos.oneDayPrice
                |> Maybe.map Encode.float
                |> Maybe.withDefault Encode.null
          )
        , ( "discountPrice"
          , bos.discountPrice
                |> Maybe.map Encode.float
                |> Maybe.withDefault Encode.null
          )
        , ( "oneWeekPrice"
          , bos.oneWeekPrice
                |> Maybe.map Encode.float
                |> Maybe.withDefault Encode.null
          )
        , ( "touristTax"
          , bos.touristTax
                |> Maybe.map Encode.float
                |> Maybe.withDefault Encode.null
          )
        , ( "options"
          , Dict.toList bos.options
                |> List.map (\( k, v ) -> ( k, encodeBookingOption v ))
                |> Encode.object
          )
        ]


encodeBookingOption : BookingOption -> Encode.Value
encodeBookingOption bo =
    Encode.object
        [ ( "name", encodeMls bo.name )
        , ( "price", Encode.float bo.price )
        , ( "key", Encode.string bo.key )
        , ( "picked", Encode.bool bo.picked )
        ]


decodeBookingInfo : Decode.Decoder BookingInfo
decodeBookingInfo =
    Decode.succeed BookingInfo
        |> required "id" Decode.int
        |> required "check_in" dateDecoder
        |> required "check_out" dateDecoder
        |> required "title" decodeTitle
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> required "address" Decode.string
        |> optional "add_address" (dJust Decode.string) Nothing
        |> required "postcode" Decode.int
        |> required "city" Decode.string
        |> required "country" Decode.string
        |> required "phone1" Decode.string
        |> optional "phone2" (dJust Decode.string) Nothing
        |> required "email" Decode.string
        |> required "nbr_adults" Decode.int
        |> optional "nbr_children" (dJust Decode.int) Nothing
        |> optional "comments" (dJust Decode.string) Nothing
        |> required "options" decodeBookingOptions
        |> required "confirmed" Decode.bool


dJust =
    Decode.map Just


strToTitle s =
    if s == "Mr" then
        Mr
    else if s == "Ms" then
        Ms
    else
        Other


decodeTitle =
    Decode.map strToTitle Decode.string


dateDecoder : Decode.Decoder Date
dateDecoder =
    Decode.int
        |> Decode.map Date.fromRataDie


getBookingOptions : (Result Error BookingOptions -> msg) -> Cmd msg
getBookingOptions responseHandler =
    Http.get
        { url = "/api/pagesdata/bookingOptions"
        , expect =
            Http.expectJson
                responseHandler
                decodeBookingOptions
        }


decodeBookingOptions =
    Decode.field "data" <|
        Decode.field "content"
            (Decode.succeed
                BookingOptions
                |> required "oneDayPrice" (Decode.nullable Decode.float)
                |> required "discountPrice" (Decode.nullable Decode.float)
                |> required "oneWeekPrice" (Decode.nullable Decode.float)
                |> required "touristTax" (Decode.nullable Decode.float)
                |> required "options" (Decode.dict decodeBookingOption)
            )


decodeBookingOption =
    Decode.succeed BookingOption
        |> required "name" decodeMls
        |> required "price" Decode.float
        |> required "key" Decode.string
        |> required "picked" Decode.bool


lockedDaysDecoder =
    Decode.map3 (\cIn cOut uuid -> { cIn = cIn, cOut = cOut, uuid = uuid })
        (Decode.field "cIn" dateDecoder)
        (Decode.field "cOut" dateDecoder)
        (Decode.field "uuid" (Decode.map Uuid.toString Uuid.decoder))


decodePresenceState jsonVal =
    Decode.decodeValue
        (Presence.presenceStateDecoder userDecoder)
        jsonVal


decodePresenceDiff jsonVal =
    Decode.decodeValue
        (Presence.presenceDiffDecoder userDecoder)
        jsonVal


userDecoder =
    Decode.field "uuid" Decode.string


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


nightsCount : Date -> Date -> Int
nightsCount d1 d2 =
    Date.diff Date.Days d1 d2


daysBooked : Date -> Date -> List Date
daysBooked d1 d2 =
    Date.range Date.Day 1 d1 d2


customerDetailView : { a | lang : Lang } -> BookingInfo -> Element msg
customerDetailView config bi =
    column
        [ spacing 15 ]
        [ el
            [ Font.bold
            ]
            (textM config.lang
                (MultLangStr "Customer details"
                    "Informations client"
                )
            )
        , row
            [ spacing 5 ]
            [ textM config.lang <|
                case bi.title of
                    Mr ->
                        MultLangStr "Mr"
                            "M."

                    Ms ->
                        MultLangStr "Ms"
                            "Mme"

                    Other ->
                        MultLangStr "Other"
                            "Autre"
            , el [] (text bi.firstName)
            , el [] (text bi.lastName)
            ]
        , paragraph [] [ text bi.address ]
        , case bi.addAddress of
            Just addAddress ->
                paragraph [] [ text addAddress ]

            _ ->
                Element.none
        , el [] (text (String.fromInt bi.postcode))
        , el [] (text bi.city)
        , el [] (text bi.country)
        ]


contactView : { a | lang : Lang } -> BookingInfo -> Element msg
contactView config bi =
    column
        [ spacing 15 ]
        [ el
            [ Font.bold
            ]
            (text "Contact")
        , el [] (text bi.phone1)
        , case bi.phone2 of
            Just phone2 ->
                el [] (text phone2)

            _ ->
                Element.none
        , el [] (text bi.email)
        ]


recapView : { a | lang : Lang } -> Date -> Date -> BookingInfo -> Element msg
recapView config cInDate cOutDate bi =
    let
        nc =
            nightsCount cInDate cOutDate
    in
    column
        [ spacing 15 ]
        [ el
            [ Font.bold
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
        , el
            []
            (text <|
                strM config.lang
                    (MultLangStr
                        "Number of adults"
                        "Nombre d'adultes"
                    )
                    ++ " : "
                    ++ String.fromInt bi.nbrAdults
            )
        , case bi.nbrKids of
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

        --, if bi.options.cleaningFee then
        --    el
        --        []
        --        (text <|
        --            strM config.lang
        --                (MultLangStr "Cleaning service: 50 €"
        --                    "Forfait ménage: 50 €"
        --                )
        --        )
        --  else
        --    Element.none
        , case bi.comment of
            Just c ->
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
                    , paragraph [] [ text c ]
                    ]

            _ ->
                Element.none
        ]
