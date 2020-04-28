module Main exposing (DisplayMode(..), Flags, Model, Msg(..), footer, header, init, main, mainMenu, subscriptions, update, urlDict, urlToDisplayMode, view)

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
import Internals.Helpers exposing (decoBorder, decodeMls, getArtworks)
import Internals.MarkdownParser as MarkdownParser
import Json.Decode as D
import Json.Encode as E
import LocalStorage.Cookies as Cookies
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
    | DisplayCookiesInfo
    | DisplayHostingInfo
    | DisplayContactInfo


type alias Model =
    { frontPage : GenericPage.Model Msg
    , accessPage : GenericPage.Model Msg
    , nearbyPage : GenericPage.Model Msg
    , galleryPage : GalleryPage.Model Msg
    , bookings : Bookings.Model Msg
    , cookiesAdmin : Cookies.Model Msg
    , ratePage : Maybe MultLangStr
    , lang : Lang
    , displayMode : DisplayMode
    , key : Nav.Key
    , url : Url.Url
    , width : Int
    , height : Int
    , currentTime : Int
    , artworks : ( String, String, String )
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
    | CookiesAdminMsg Cookies.Msg
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

        ( newCookiesAdmin, cookiesAdminCmd ) =
            Cookies.init CookiesAdminMsg

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
      , cookiesAdmin = newCookiesAdmin
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
      , artworks = getArtworks flags.currentTime
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
        , cookiesAdminCmd
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
        , Cookies.subscriptions model.cookiesAdmin
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

        CookiesAdminMsg cookiesAdminMsg ->
            let
                ( newCookiesAdmin, cookiesAdminCmd ) =
                    Cookies.update cookiesAdminMsg model.cookiesAdmin
            in
            ( { model | cookiesAdmin = newCookiesAdmin }
            , cookiesAdminCmd
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
        , ( "cookies", DisplayCookiesInfo )
        , ( "hosting", DisplayHostingInfo )
        , ( "contact", DisplayContactInfo )
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
    let
        ( a1, a2, a3 ) =
            model.artworks
    in
    { title = "Le vieux Lilas"
    , body =
        [ Element.layout
            [ width fill
            , Font.size 16
            , if model.displayMode /= DisplayCookiesInfo then
                inFront
                    (Cookies.floatingDialogView
                        { lang = model.lang
                        , width = model.width
                        }
                        model.cookiesAdmin
                    )

              else
                noAttr
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
                                    , showGoogleMaps = .googleConsent <| Cookies.cookiesPrefs model.cookiesAdmin
                                    }
                                    model.frontPage

                            DisplayDetails ->
                                GalleryPage.view
                                    { lang = model.lang
                                    , url = model.url
                                    , width = model.width
                                    , artwork = a1
                                    }
                                    model.galleryPage

                            DisplayBookings ->
                                Bookings.view
                                    { lang = model.lang
                                    , url = model.url
                                    , width = model.width
                                    , artwork = a2
                                    , canUseGoogleRecaptcha = .googleConsent <| Cookies.cookiesPrefs model.cookiesAdmin
                                    }
                                    model.bookings

                            DisplayRates ->
                                case model.ratePage of
                                    Nothing ->
                                        Element.none

                                    Just page ->
                                        column
                                            [ width (maximum 1000 fill)
                                            , height (minimum 500 fill)
                                            , centerX
                                            , padding 15
                                            , spacing 15
                                            ]
                                            [ el
                                                []
                                                (MarkdownParser.renderMarkdown
                                                    (strM model.lang page)
                                                    DownloadDoc
                                                )
                                            , image
                                                [ centerX
                                                , width (px <| min 800 (model.width - 30))
                                                ]
                                                { src = decoBorder
                                                , description = ""
                                                }
                                            , el
                                                [ width (px <| min 500 model.width)
                                                , height (px <| min 500 model.width)
                                                , Background.uncropped a3
                                                , centerX
                                                ]
                                                (text "")
                                            ]

                            DisplayNearby ->
                                GenericPage.view
                                    { lang = model.lang
                                    , width = model.width
                                    , showGoogleMaps = .googleConsent <| Cookies.cookiesPrefs model.cookiesAdmin
                                    }
                                    model.nearbyPage

                            DisplayAccess ->
                                GenericPage.view
                                    { lang = model.lang
                                    , width = model.width
                                    , showGoogleMaps = .googleConsent <| Cookies.cookiesPrefs model.cookiesAdmin
                                    }
                                    model.accessPage

                            DisplayCookiesInfo ->
                                Cookies.view
                                    { lang = model.lang
                                    , width = model.width
                                    }
                                    model.cookiesAdmin

                            DisplayHostingInfo ->
                                displayHostingInfo model

                            DisplayContactInfo ->
                                displayContactInfo model
                        ]
                    , footer model
                    ]
                )
            )
        ]
    }


displayHostingInfo model =
    column
        [ spacing 40
        , padding 15
        , width (maximum 1000 fill)
        , centerX
        , Font.size 18
        , Font.family
            [ Font.typeface "times"
            ]
        ]
        [ el
            [ Font.bold
            , Font.size 22
            , Font.family
                [ Font.typeface "Crimson Text"
                , Font.sansSerif
                ]
            ]
            (textM model.lang
                (MultLangStr "Hosting and data policy"
                    "Hébergement et gestion des données"
                )
            )
        , column
            [ spacing 5 ]
            [ el [] (text "Conception: Gillard Informatique")
            , el
                []
                (text "5 Place de l'église")
            , el
                []
                (text "89520 Lainsecq")
            , el
                []
                (text "Tel +33 (0)3 86 74 72 64")
            , el
                []
                (text "Mobile +33 (0)6 52 11 05 72")
            , el
                []
                (text "Siret: 823 705 009 00020")
            , row
                [ spacing 10 ]
                [ el [] (text "Site officiel:")
                , newTabLink
                    [ mouseOver
                        [ Font.color blue ]
                    , Font.underline
                    , Font.color lightBlue
                    ]
                    { url = "http://www.gillardinformatique.net"
                    , label = text "gillardinformatique.net"
                    }
                ]
            ]
        , paragraph
            []
            [ textM model.lang
                (MultLangStr
                    "This website is hosted by "
                    "Ce site est hébergé par la société "
                )
            , link
                [ mouseOver
                    [ Font.color blue ]
                , Font.underline
                , Font.color lightBlue
                ]
                { url = "https://www.heroku.com"
                , label =
                    textM model.lang
                        (MultLangStr
                            "Heroku"
                            "Heroku"
                        )
                }
            ]
        , paragraph
            []
            [ textM model.lang
                (MultLangStr
                    "Personal data collected via the booking form are only used internaly. This data will not be shared or sold to anyone"
                    "Les données personnelles collectées par ce site via le formulaire de réservation sont uniquement destinées à un usage interne. En aucun cas ces données ne seront cédées, communiquées ou vendues à des tiers. "
                )
            ]
        , paragraph []
            [ textM
                config.lang
                (MultLangStr
                    "We garantee we will only store customer data for the time necessary to process the transaction."
                    "Nous nous engageons à conserver les données du client pendant la période strictement nécessaire pour la finalité du traitement."
                )
            ]
        ]


displayContactInfo model =
    column
        [ spacing 40
        , padding 15
        , width (maximum 1000 fill)
        , centerX
        , Font.size 18
        , Font.family
            [ Font.typeface "times"
            ]
        ]
        [ el
            [ Font.bold
            , Font.size 22
            , Font.family
                [ Font.typeface "Crimson Text"
                , Font.sansSerif
                ]
            ]
            (textM model.lang
                (MultLangStr "Contact"
                    "Contact"
                )
            )
        , column
            [ spacing 10 ]
            [ el
                [ Font.size 20
                , Font.family
                    [ Font.typeface "Crimson Text"
                    , Font.sansSerif
                    ]
                ]
                (textM model.lang (MultLangStr "" "Responsable gîte:"))
            , el [ Font.bold ] (text "Sylvie Gillard")
            , text "sylvie.gillard@laposte.net"
            ]
        , column
            [ spacing 10 ]
            [ el
                [ Font.size 20
                , Font.family
                    [ Font.typeface "Crimson Text"
                    , Font.sansSerif
                    ]
                ]
                (textM model.lang (MultLangStr "" "Responsable site:"))
            , el [ Font.bold ] (text "Florian Gillard")
            , text "florian.gillard@tutanota.com"
            ]
        ]


header : Model -> Element Msg
header model =
    let
        isDesktop =
            model.width > 1000

        headerHeight =
            if isDesktop then
                325

            else
                120

        titleFontsize =
            if isDesktop then
                65

            else
                40

        quoteFontsize =
            32

        backgroundSize =
            if isDesktop then
                300

            else
                100

        langSelectView =
            column
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

        mainTitle =
            el
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
                , centerX
                ]
                (text "Le Vieux Lilas")

        headerBody =
            row
                [ centerX
                , moveUp 50
                , spacing 15
                ]
                [ quoteView
                , logoView
                ]

        quoteView =
            column
                [ spacing 25
                ]
                [ column
                    [ spacing 5
                    , Font.family
                        [ Font.typeface "Great Vibes"
                        , Font.serif
                        ]
                    , Font.size quoteFontsize
                    , Font.color darkCharcoal
                    ]
                    [ el [ centerX ] (text "Vois\u{200A}, arrête-toi\u{200A}, cet instant est beau\u{200A}!")
                    , el [ centerX ] (text "Y a-t-il ailleurs\u{200A}, dans toute ta vie qui se précipite\u{200A},")
                    , el [ centerX ] (text "un soleil aussi blond\u{200A}, un lilas aussi bleu à force d'être mauve\u{200A}…\u{200A}?")
                    ]
                , row
                    [ spacing 10
                    , Font.family
                        [ Font.typeface "times"
                        , Font.serif
                        ]
                    , Font.size <| quoteFontsize - 14
                    , Font.color darkCharcoal
                    , alignRight
                    ]
                    [ el [ Font.italic ]
                        (text "La retraite sentimentale,")
                    , el [] (text "Colette")
                    ]
                ]

        logoView =
            el
                [ Background.uncropped "/images/lilacW.png"
                , Background.color lightGrey
                , width (px backgroundSize)
                , height (px backgroundSize)
                ]
                Element.none
    in
    column
        [ width fill
        , height (px headerHeight)
        , Background.tiled "/images/canvas.png"
        , behindContent
            (el
                [ Background.color white
                , alpha 0.5
                , width fill
                , height (px headerHeight)
                , paddingEach { sides | top = 15 }
                ]
                Element.none
            )
        ]
        (if isDesktop then
            [ langSelectView
            , mainTitle
            , headerBody
            ]

         else
            [ row
                [ width fill
                , paddingXY 15 0
                , centerY
                ]
                [ mainTitle
                , el [ alignRight ] logoView
                ]
            ]
        )


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
                { fr = "Mentions légales"
                , en = "Legal & privacy"
                }
            , footerItem
                { fr = "Hébergement et traitement des données"
                , en = "Hosting and data policy"
                }
                "/hosting"
            , footerItem
                { fr = "Gestion des cookies"
                , en = "Cookies"
                }
                "/cookies"
            , footerItem
                { fr = "Conditions générales de vente"
                , en = "Conditions générales de vente"
                }
                "https://gite-vieux-lilas.s3.eu-west-3.amazonaws.com/Documents/CONDITIONS_GENERALES.pdf"
            , footerItem
                { fr = "Contact"
                , en = "Contact"
                }
                "/contact"
            ]
        ]
