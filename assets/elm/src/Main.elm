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
import FrontPage.FrontPage as FrontPage
import Html.Attributes as HtmlAttr
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
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
    { frontPage : FrontPage.Model Msg
    , bookings : Bookings.Model Msg
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
    | FrontPageMsg FrontPage.Msg
    | BookingsMsg Bookings.Msg
    | ChangeLang Lang
    | NoOp


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    , seedInfo : ( Int, List Int )
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( newBookings, bookingsCmd ) =
            Bookings.init BookingsMsg flags.seedInfo

        ( newFrontPage, frontPageCmd ) =
            FrontPage.init FrontPageMsg

        url_ =
            if
                (url.path == "/")
                    || (urlToDisplayMode url == Nothing)
            then
                { url | path = "/home" }
            else
                url
    in
        ( { frontPage = newFrontPage
          , bookings = newBookings
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
            , frontPageCmd
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

        FrontPageMsg frontPageMsg ->
            let
                newFrontPage =
                    FrontPage.update frontPageMsg model.frontPage
            in
                ( { model | frontPage = newFrontPage }
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
                                FrontPage.view
                                    { lang = model.lang
                                    , width = model.width
                                    }
                                    model.frontPage

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
    let
        isDesktop =
            model.width > 1000

        alphaLayerHeight =
            if isDesktop then
                325
            else
                170

        titleFontsize =
            if isDesktop then
                65
            else
                32

        titleOffset =
            if isDesktop then
                60
            else
                30

        titleContainerWidth =
            if isDesktop then
                400
            else
                200

        backgroundSize =
            if isDesktop then
                300
            else
                150
    in
        column
            [ width fill
            , Background.tiled "/images/canvas.png"
            , behindContent
                (el
                    [ Background.color white
                    , alpha 0.5
                    , width fill
                    , height (px alphaLayerHeight)
                    , paddingEach { sides | top = 15 }
                    ]
                    Element.none
                )
            ]
            [ column
                [ alignRight
                , alignTop
                , moveLeft 15
                , moveDown 15
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
            , row
                [ width (px titleContainerWidth)
                  --, Background.color red
                , centerX
                , inFront
                    (el
                        [ Font.family
                            [ Font.typeface "Great Vibes"
                            , Font.serif
                            ]
                        , Font.size titleFontsize
                        , Font.color darkCharcoal
                        , Font.shadow
                            { offset = ( 1, 1 )
                            , blur = 0
                            , color = white
                            }
                        , Font.family
                            [ Font.typeface "Great Vibes"
                            , Font.serif
                            ]
                        , moveLeft titleOffset
                        ]
                        (text "Le Vieux Lilas")
                    )
                ]
                [ el
                    [ Background.uncropped "/images/lilacW.png"
                    , Background.color lightGrey
                    , width (px backgroundSize)
                    , height (px backgroundSize)
                    , centerX
                    , paddingEach { sides | top = 15 }
                    , alignRight
                    ]
                    Element.none
                ]
            ]


mainMenu : Model -> Element Msg
mainMenu model =
    let
        isMobile =
            model.width < 1000

        menuItem mls url =
            link
                [ padding 15
                , centerX
                , mouseOver
                    [ Background.color darkYellow
                    ]
                , Font.family
                    [ Font.typeface "Great Vibes"
                    , Font.serif
                    ]
                , Font.size 30
                , Font.color lightGray
                , if isMobile then
                    width fill
                  else
                    noAttr
                ]
                { url = url
                , label =
                    el [] (textM model.lang mls)
                }
    in
        (if isMobile then
            column
         else
            row
        )
            [ width fill
            , Background.color darkGreen
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
    let
        isMobile =
            model.width < 1000

        footerHeader mls =
            el
                [ paddingEach { sides | bottom = 20 }
                , Font.size 15
                , Font.bold
                , Font.color white
                ]
                (textM model.lang mls)

        footerItem mls url =
            link
                [ mouseOver
                    [ Font.color white
                    ]
                , Font.family
                    [ Font.typeface "Lota"
                    , Font.sansSerif
                    ]
                , Font.size 14
                , Font.color grey
                , if isMobile then
                    width fill
                  else
                    noAttr
                ]
                { url = url
                , label =
                    el [] (textM model.lang mls)
                }
    in
        (if isMobile then
            column
         else
            row
        )
            [ width fill
            , Background.color charcoal
            , padding 45
            , if isMobile then
                spacing 50
              else
                spacing 150
            ]
            [ column
                [ alignTop
                , spacing 5
                , centerX
                ]
                [ footerHeader
                    { fr = "Plan de site"
                    , en = "Site map"
                    }
                , footerItem
                    { fr = "Accueil"
                    , en = "Home"
                    }
                    "/home"
                , footerItem
                    { fr = "Notre gîte"
                    , en = "Our gîte"
                    }
                    "/details"
                , footerItem
                    { fr = "Les tarifs"
                    , en = "Rates"
                    }
                    "/rates"
                , footerItem
                    { fr = "Réservations"
                    , en = "Booking"
                    }
                    "/bookings"
                , footerItem
                    { fr = "Accès"
                    , en = "Access"
                    }
                    "/access"
                , footerItem
                    { fr = "Dans les environs"
                    , en = "Nearby interests"
                    }
                    "/nearby"
                ]
            , column
                [ alignTop
                , spacing 5
                , centerX
                ]
                [ footerHeader
                    { fr = "Réalisation"
                    , en = "Conception"
                    }
                , footerItem
                    { fr = "Gillard Informatique"
                    , en = "Gillard Informatique"
                    }
                    "http://www.gillardinformatique.net"
                , image
                    []
                    { src = "/images/logo.png"
                    , description = "Gillard Informatique"
                    }
                ]
            , column
                [ alignTop
                , spacing 5
                , centerX
                ]
                [ footerHeader
                    { fr = "Hébergement"
                    , en = "Hosting"
                    }
                , footerItem
                    { fr = "Heroku"
                    , en = "Heroku"
                    }
                    "https://www.heroku.com/"
                ]
            ]
