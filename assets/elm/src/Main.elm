module Main exposing (..)

import Browser exposing (application, UrlRequest(..))
import Browser.Navigation as Nav
import Bookings.Bookings as Bookings
import Style.Helpers as StyleHelpers
import Style.Icons as Icons
import Style.Palette as Palette
import Json.Decode as D
import Json.Encode as E
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import MultLang.MultLang exposing (..)
import Url as Url
import Html.Attributes as HtmlAttr
import Browser.Events exposing (onResize)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangeUrl
        }


type DisplayMode
    = DisplayFrontPage
    | DisplayBookings
    | DisplayRates
    | DisplayNearby
    | DisplayAccess


type alias Model =
    { bookings : Bookings.Model Msg
    , lang : Lang
    , displayMode : DisplayMode
    , key : Nav.Key
    , url : Url.Url
    , width : Int
    , height : Int
    , currentTime : Int
    }


type Msg
    = ChangeUrl Url.Url
    | ClickedLink UrlRequest
    | WinResize Int Int
    | BookingsMsg Bookings.Msg
    | NoOp


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( newBookings, bookingsCmd ) =
            Bookings.init BookingsMsg
    in
        ( { bookings = newBookings
          , lang = English
          , displayMode = DisplayBookings
          , key = key
          , url = url
          , width =
                flags.width
          , height =
                flags.height
          , currentTime =
                flags.currentTime
          }
        , Cmd.batch
            [ bookingsCmd ]
        )


subscriptions model =
    Sub.batch
        ([ onResize WinResize
         ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl
                        model.key
                        (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        ChangeUrl url ->
            ( { model | url = url }
            , Cmd.none
            )

        WinResize width height ->
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )

        BookingsMsg bookingsMsg ->
            let
                ( newBookings, bookingsCmd ) =
                    Bookings.update bookingsMsg model.bookings
            in
                ( { model | bookings = newBookings }
                , bookingsCmd
                )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Le vieux Lilas"
    , body =
        [ Element.layout
            [ width fill
            , Font.size 16
            ]
            (el
                [ width fill
                , height (px model.height)
                , clip
                  --, Background.image ""
                ]
                (column
                    [ width fill
                    , scrollbarY
                    , htmlAttribute <| HtmlAttr.style "id" "mainContainer"
                    ]
                    [ case model.displayMode of
                        DisplayFrontPage ->
                            Element.none

                        DisplayBookings ->
                            Bookings.view
                                { lang = model.lang }
                                model.bookings

                        DisplayRates ->
                            Element.none

                        DisplayNearby ->
                            Element.none

                        DisplayAccess ->
                            Element.none
                    ]
                )
            )
        ]
    }
