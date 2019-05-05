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
import File.Download as Download
import Gallery.GalleryPage as GalleryPage
import GenericPage.GenericPage as GenericPage
import Html.Attributes as HtmlAttr
import Http exposing (..)
import Internals.Helpers exposing (decodeMls)
import Internals.MarkdownParser as MarkdownParser
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
    { frontPage : GenericPage.Model Msg
    , accessPage : GenericPage.Model Msg
    , nearbyPage : GenericPage.Model Msg
    , galleryPage : GalleryPage.Model Msg
    , bookings : Bookings.Model Msg
    , ratePage : Maybe MultLangStr
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
    | FrontPageMsg GenericPage.Msg
    | AccessPageMsg GenericPage.Msg
    | NearbyPageMsg GenericPage.Msg
    | GalleryPageMsg GalleryPage.Msg
    | BookingsMsg Bookings.Msg
    | ChangeLang Lang
    | GotRateArticle (Result Http.Error MultLangStr)
    | DownloadDoc String
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
            Bookings.init BookingsMsg flags.seedInfo False

        ( newFrontPage, frontPageCmd ) =
            GenericPage.init FrontPageMsg "frontPage"

        ( newAccessPage, accessPageCmd ) =
            GenericPage.init AccessPageMsg "access"

        ( newNearbyPage, nearbyPageCmd ) =
            GenericPage.init NearbyPageMsg "nearby"

        ( newGalleryPage, galleryCmd ) =
            GalleryPage.init GalleryPageMsg

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
      , accessPage = newAccessPage
      , nearbyPage = newNearbyPage
      , galleryPage = newGalleryPage
      , bookings = newBookings
      , ratePage = Nothing
      , lang = French
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
        , accessPageCmd
        , nearbyPageCmd
        , galleryCmd
        , bookingsCmd
        , Http.get
            { url = "/api/pagesdata/rateArticle"
            , expect =
                Http.expectJson
                    GotRateArticle
                    (D.field "data" <|
                        D.field "content"
                            decodeMls
                    )
            }
        ]
    )


subscriptions model =
    Sub.batch
        [ onResize WinResize
        , GenericPage.subscriptions model.frontPage
        , GenericPage.subscriptions model.accessPage
        , GenericPage.subscriptions model.nearbyPage
        , Bookings.subscriptions model.bookings
        , GalleryPage.subscriptions model.galleryPage
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
                ( newFrontPage, cmd ) =
                    GenericPage.update { width = model.width } frontPageMsg model.frontPage
            in
            ( { model | frontPage = newFrontPage }
            , cmd
            )

        AccessPageMsg accessPageMsg ->
            let
                ( newAccessPage, cmd ) =
                    GenericPage.update { width = model.width } accessPageMsg model.accessPage
            in
            ( { model | accessPage = newAccessPage }
            , cmd
            )

        NearbyPageMsg nearbyPageMsg ->
            let
                ( newNearbyPage, cmd ) =
                    GenericPage.update { width = model.width } nearbyPageMsg model.nearbyPage
            in
            ( { model | nearbyPage = newNearbyPage }
            , cmd
            )

        GalleryPageMsg galleryPageMsg ->
            let
                ( newGalleryPage, cmd ) =
                    GalleryPage.update
                        { width = model.width
                        , lang = model.lang
                        , key = model.key
                        }
                        galleryPageMsg
                        model.galleryPage
            in
            ( { model | galleryPage = newGalleryPage }
            , cmd
            )

        BookingsMsg bookingsMsg ->
            let
                ( newBookings, bookingsCmd ) =
                    Bookings.update { key = model.key } bookingsMsg model.bookings
            in
            ( { model | bookings = newBookings }
            , bookingsCmd
            )

        ChangeLang l ->
            ( { model | lang = l }, Cmd.none )

        GotRateArticle res ->
            case res of
                Ok a ->
                    ( { model | ratePage = Just a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DownloadDoc url ->
            ( model, Download.url url )

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
                                GenericPage.view
                                    { lang = model.lang
                                    , width = model.width
                                    }
                                    model.frontPage

                            DisplayDetails ->
                                GalleryPage.view
                                    { lang = model.lang
                                    , url = model.url
                                    , width = model.width
                                    }
                                    model.galleryPage

                            DisplayBookings ->
                                Bookings.view
                                    { lang = model.lang
                                    , url = model.url
                                    , width = model.width
                                    }
                                    model.bookings

                            DisplayRates ->
                                case model.ratePage of
                                    Nothing ->
                                        Element.none

                                    Just page ->
                                        el
                                            [ width (maximum 1000 fill)
                                            , height (minimum 500 fill)
                                            , centerX
                                            , padding 15
                                            , spacing 15
                                            ]
                                            (MarkdownParser.renderMarkdown
                                                (strM model.lang page)
                                                DownloadDoc
                                            )

                            DisplayNearby ->
                                GenericPage.view
                                    { lang = model.lang
                                    , width = model.width
                                    }
                                    model.nearbyPage

                            DisplayAccess ->
                                GenericPage.view
                                    { lang = model.lang
                                    , width = model.width
                                    }
                                    model.accessPage
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
                            "/images/french.png"

                        French ->
                            "/images/english.png"
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
