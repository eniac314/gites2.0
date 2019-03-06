module Main exposing (..)

import Bookings.Bookings as Bookings
import Browser exposing (UrlRequest(..), application)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Html.Attributes as HtmlAttr
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers as StyleHelpers
import Style.Icons as Icons
import Style.Palette exposing (..)
import Url as Url


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
    | DisplayDetails
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
    | ChangeLang Lang
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

        url_ =
            if
                (url.path == "/")
                    || (urlToDisplayMode url == Nothing)
            then
                { url | path = "/home" }
            else
                url
    in
    ( { bookings = newBookings
      , lang = English
      , displayMode =
            urlToDisplayMode url_
                |> Maybe.withDefault DisplayFrontPage
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
        [ if url /= url_ then
            Nav.pushUrl key (Url.toString url_)
          else
            Cmd.none
        , bookingsCmd
        ]
    )


subscriptions model =
    Sub.batch
        [ onResize WinResize
        , Bookings.subscriptions model.bookings
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , if urlToDisplayMode url == Nothing then
                        Cmd.none
                      else
                        Nav.pushUrl
                            model.key
                            (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        ChangeUrl url ->
            ( { model
                | url = url
                , displayMode =
                    urlToDisplayMode url
                        |> Maybe.withDefault model.displayMode
              }
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

        ChangeLang l ->
            ( { model | lang = l }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------


urlDict : Dict String DisplayMode
urlDict =
    Dict.fromList
        [ ( "home", DisplayFrontPage )
        , ( "details", DisplayDetails )
        , ( "bookings", DisplayBookings )
        , ( "rates", DisplayRates )
        , ( "nearby", DisplayNearby )
        , ( "access", DisplayAccess )
        ]


urlToDisplayMode : Url.Url -> Maybe DisplayMode
urlToDisplayMode url =
    case String.split "/" url.path of
        "" :: root :: xs ->
            Dict.get root urlDict

        _ ->
            Nothing



-------------------------------------------------------------------------------


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
                , height fill

                --(px model.height)
                --, clip
                , Background.tiled "/images/vintage-concreteS.png"
                ]
                (column
                    [ width fill

                    --, scrollbarY
                    , htmlAttribute <| HtmlAttr.style "id" "mainContainer"
                    ]
                    [ header model
                    , mainMenu model
                    , column
                        [ width (maximum 1000 fill)
                        , height (minimum 500 fill)
                        , centerX
                        ]
                        [ case model.displayMode of
                            DisplayFrontPage ->
                                Element.none

                            DisplayDetails ->
                                Element.none

                            DisplayBookings ->
                                Bookings.view
                                    { lang = model.lang
                                    , url = model.url
                                    , width = model.width
                                    }
                                    model.bookings

                            DisplayRates ->
                                Element.none

                            DisplayNearby ->
                                Element.none

                            DisplayAccess ->
                                Element.none
                        ]
                    , footer model
                    ]
                )
            )
        ]
    }


header : Model -> Element Msg
header model =
    column
        [ width fill
        , Background.color white
        ]
        [ column
            [ alignRight
            , alignTop
            ]
            [ image
                [ width (px 30)
                , Events.onClick
                    (if model.lang == French then
                        ChangeLang English
                     else
                        ChangeLang French
                    )
                , pointer
                ]
                { src =
                    case model.lang of
                        English ->
                            "/images/english.png"

                        French ->
                            "/images/french.png"
                , description = ""
                }
            ]

        --, image
        --    [ centerX
        --    , width (px 300)
        --    ]
        --    { src = "/images/lilas.png"
        --    , description = "Le vieux lilas"
        --    }
        , el
            [ Background.uncropped "/images/lilas.png"
            , Background.color lightGrey
            , width (px 300)
            , height (px 300)
            , centerX
            ]
            Element.none
        , el
            [ centerX
            , Font.center
            , Font.family
                [ Font.typeface "Great Vibes"
                , Font.serif
                ]
            , paddingXY 0 0
            , Font.size 60
            ]
            (text "Le Vieux Lilas")
        ]


mainMenu : Model -> Element Msg
mainMenu model =
    let
        menuItem mls url =
            link
                [ padding 20
                , centerX
                , mouseOver [ Background.color lightGreen ]
                ]
                { url = url
                , label =
                    el [] (textM model.lang mls)
                }
    in
    (if model.width < 1000 then
        column
     else
        row
    )
        [ width fill
        , Background.color green
        ]
        [ menuItem
            { fr = "Accueil"
            , en = "Home"
            }
            "/home"
        , menuItem
            { fr = "Notre gîte"
            , en = "Our gîte"
            }
            "/details"
        , menuItem
            { fr = "Les tarifs"
            , en = "Rates"
            }
            "/rates"
        , menuItem
            { fr = "Réservations"
            , en = "Booking"
            }
            "/bookings"
        , menuItem
            { fr = "Accès"
            , en = "Access"
            }
            "/access"
        , menuItem
            { fr = "Dans les environs"
            , en = "Nearby interests"
            }
            "/nearby"
        ]


footer : Model -> Element msg
footer model =
    row
        []
        []
