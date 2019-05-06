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
    , pets : Maybe String
    , options : BookingOptions
    , language : Lang
    , confirmed : Bool
    }


type alias BookingOptions =
    { twoNightsPrice : Maybe Float
    , threeNightsPrice : Maybe Float
    , moreThan3NightsPrice : Maybe Float
    , touristTax : Maybe Float
    , options : Dict String BookingOption
    }


dummyOptions =
    { twoNightsPrice = Nothing
    , threeNightsPrice = Nothing
    , moreThan3NightsPrice = Nothing
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
        , ( "pets"
          , bookingInfo.pets
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "options"
          , encodeBookingOptions bookingInfo.options
          )
        , ( "language"
          , case bookingInfo.language of
                French ->
                    Encode.string "french"

                English ->
                    Encode.string "english"
          )
        , ( "confirmed"
          , Encode.bool bookingInfo.confirmed
          )
        ]


encodeBookingOptions : BookingOptions -> Encode.Value
encodeBookingOptions bos =
    Encode.object
        [ ( "twoNightsPrice"
          , bos.twoNightsPrice
                |> Maybe.map Encode.float
                |> Maybe.withDefault Encode.null
          )
        , ( "threeNightsPrice"
          , bos.threeNightsPrice
                |> Maybe.map Encode.float
                |> Maybe.withDefault Encode.null
          )
        , ( "moreThan3NightsPrice"
          , bos.moreThan3NightsPrice
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
        |> optional "pets" (dJust Decode.string) Nothing
        |> required "options" decodeBookingOptions
        |> required "language" decodeLanguage
        |> required "confirmed" Decode.bool


dJust =
    Decode.map Just


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
                decodeBookingOptionServer
        }


decodeBookingOptionServer =
    Decode.field "data" <|
        Decode.field "content"
            decodeBookingOptions


decodeLanguage =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "french" ->
                        Decode.succeed French

                    "english" ->
                        Decode.succeed English

                    _ ->
                        Decode.succeed French
            )


decodeBookingOptions =
    Decode.succeed
        BookingOptions
        |> required "twoNightsPrice" (Decode.nullable Decode.float)
        |> required "threeNightsPrice" (Decode.nullable Decode.float)
        |> required "moreThan3NightsPrice" (Decode.nullable Decode.float)
        |> required "touristTax" (Decode.nullable Decode.float)
        |> required "options" (Decode.dict decodeBookingOption)


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
            [ el [] (text bi.firstName)
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


recapView : { a | lang : Lang } -> Date -> Date -> BookingInfo -> BookingOptions -> Element msg
recapView config cInDate cOutDate bi opt =
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
        , priceView config.lang nc bi.nbrAdults (Maybe.withDefault 0 bi.nbrKids) opt
        , case bi.pets of
            Just s ->
                paragraph
                    []
                    [ text <| (strM config.lang (MultLangStr "Pets: " "Animaux: ") ++ s) ]

            Nothing ->
                Element.none
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


priceView lang nc na nk bo =
    let
        p1 =
            bo.twoNightsPrice
                |> Maybe.withDefault 0

        p2 =
            bo.threeNightsPrice
                |> Maybe.withDefault 0

        p3 =
            bo.moreThan3NightsPrice
                |> Maybe.withDefault 0

        basePrice =
            if nc == 2 then
                p1
            else if nc == 3 then
                p2
            else
                toFloat nc * p3

        opts =
            bo.options
                |> Dict.values
                |> List.filter .picked
                |> List.foldr
                    (\o acc -> o.price + acc)
                    0

        tax =
            let
                taxBase =
                    ((basePrice / (toFloat <| na + nk)) * 0.05) * (toFloat <| na * nc)
            in
            taxBase + (0.1 * taxBase)
    in
    el
        []
        (text <|
            "Total: "
                ++ String.fromFloat (basePrice + opts + tax)
                ++ " €"
        )



-------------------------------------------------------------------------
