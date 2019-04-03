module Admin exposing (..)

import Auth.AuthPlugin as Auth
import Bookings.BookingsAdmin as BookingsAdmin
import Bookings.OptionsAdmin as OptionsAdmin
import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import File exposing (..)
import File.Select as Select
import Gallery.GalleryAdmin as GalleryAdmin
import GenericPage.GenericPageAdmin as GenericPageAdmin
import Http exposing (expectString)
import Internals.Helpers exposing (..)
import Internals.Uploader as Uploader
import MultLang.MultLang exposing (..)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Style.Helpers exposing (sides, tabView)
import Task exposing (perform)
import Time exposing (here)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    , seedInfo : ( Int, List Int )
    }


type alias Model =
    { displayMode : DisplayMode
    , lang : Lang
    , width : Int
    , height : Int
    , currentTime : Int
    , zone : Time.Zone
    , authPlugin : Auth.Model Msg
    , frontPageAdmin : GenericPageAdmin.Model Msg
    , accessPageAdmin : GenericPageAdmin.Model Msg
    , nearbyPageAdmin : GenericPageAdmin.Model Msg
    , galleryAdmin : GalleryAdmin.Model Msg
    , bookingsAdmin : BookingsAdmin.Model Msg
    }


type Msg
    = AuthMsg Auth.Msg
    | FrontPageAdminMsg GenericPageAdmin.Msg
    | AccessPageAdminMsg GenericPageAdmin.Msg
    | NearbyPageAdminMsg GenericPageAdmin.Msg
    | GalleryAdminMsg GalleryAdmin.Msg
    | BookingAdminMsg BookingsAdmin.Msg
    | SetDisplayMode DisplayMode
    | WinResize Int Int
    | SetZone Time.Zone
    | ChangeLang Lang
    | NoOp


type DisplayMode
    = DisplayAuth
    | DisplayFrontPageAdmin
    | DisplayGalleryAdmin
    | DisplayBookingsAdmin
    | DisplayAccessAdmin
    | DisplayNearbyAdmin
    | DisplayRatesAdmin


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize WinResize
        , Auth.subscriptions model.authPlugin
        , GenericPageAdmin.subscriptions model.frontPageAdmin
        , GenericPageAdmin.subscriptions model.accessPageAdmin
        , GenericPageAdmin.subscriptions model.nearbyPageAdmin
        , GalleryAdmin.subscriptions model.galleryAdmin
        , BookingsAdmin.subscriptions model.bookingsAdmin
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( seed, seedExtension ) =
            flags.seedInfo

        ( uuid1, seed1 ) =
            step Uuid.stringGenerator (initialSeed seed seedExtension)

        ( uuid2, seed2 ) =
            step Uuid.stringGenerator seed1

        ( uuid3, seed3 ) =
            step Uuid.stringGenerator seed2

        ( newFrontPageAdmin, fpaCmd ) =
            GenericPageAdmin.init FrontPageAdminMsg "frontPage" ( uuid1, seed1 )

        ( newAccessPageAdmin, accPgCmd ) =
            GenericPageAdmin.init AccessPageAdminMsg "access" ( uuid2, seed2 )

        ( newNearbyPageAdmin, neaPgCmd ) =
            GenericPageAdmin.init NearbyPageAdminMsg "nearby" ( uuid3, seed3 )

        ( newGalleryAdmin, gAdCmd ) =
            GalleryAdmin.init GalleryAdminMsg

        ( newBookingAdmin, bkAdCmd ) =
            BookingsAdmin.init BookingAdminMsg

        ( newAuthPlugin, authPluginCmd ) =
            Auth.init AuthMsg
    in
    ( { displayMode = DisplayAuth
      , lang = French
      , width =
            flags.width
      , height =
            flags.height
      , currentTime =
            flags.currentTime
      , zone = Time.utc
      , authPlugin = newAuthPlugin
      , frontPageAdmin = newFrontPageAdmin
      , accessPageAdmin = newAccessPageAdmin
      , nearbyPageAdmin = newNearbyPageAdmin
      , galleryAdmin = newGalleryAdmin
      , bookingsAdmin = newBookingAdmin
      }
    , Cmd.batch
        [ Task.perform SetZone Time.here
        , authPluginCmd
        , fpaCmd
        , accPgCmd
        , neaPgCmd
        , gAdCmd
        , bkAdCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsg authPluginMsg ->
            let
                logInfo =
                    Auth.getLogInfo newAuthPlugin

                ( newAuthPlugin, authToolCmds, mbPluginResult ) =
                    Auth.update authPluginMsg model.authPlugin
            in
            ( { model
                | authPlugin = newAuthPlugin
                , displayMode =
                    if mbPluginResult == Just PluginQuit then
                        DisplayFrontPageAdmin
                    else
                        model.displayMode
              }
            , Cmd.batch <|
                [ authToolCmds ]
            )

        FrontPageAdminMsg fpaMsg ->
            let
                ( newFrontPageAdmin, cmd ) =
                    GenericPageAdmin.update
                        { logInfo = model.authPlugin.logInfo
                        , width = model.width
                        }
                        fpaMsg
                        model.frontPageAdmin
            in
            ( { model | frontPageAdmin = newFrontPageAdmin }
            , cmd
            )

        AccessPageAdminMsg accPgMsg ->
            let
                ( newAccessPageAdmin, cmd ) =
                    GenericPageAdmin.update
                        { logInfo = model.authPlugin.logInfo
                        , width = model.width
                        }
                        accPgMsg
                        model.accessPageAdmin
            in
            ( { model | accessPageAdmin = newAccessPageAdmin }
            , cmd
            )

        NearbyPageAdminMsg neaPgMsg ->
            let
                ( newNearbyPageAdmin, cmd ) =
                    GenericPageAdmin.update
                        { logInfo = model.authPlugin.logInfo
                        , width = model.width
                        }
                        neaPgMsg
                        model.nearbyPageAdmin
            in
            ( { model | nearbyPageAdmin = newNearbyPageAdmin }
            , cmd
            )

        GalleryAdminMsg gAdMsg ->
            let
                ( newGalleryAdmin, cmd ) =
                    GalleryAdmin.update
                        { logInfo = model.authPlugin.logInfo
                        , width = model.width
                        }
                        gAdMsg
                        model.galleryAdmin
            in
            ( { model | galleryAdmin = newGalleryAdmin }
            , cmd
            )

        BookingAdminMsg bkAdMsg ->
            let
                ( newBookingAdmin, cmd ) =
                    BookingsAdmin.update
                        { logInfo = model.authPlugin.logInfo }
                        bkAdMsg
                        model.bookingsAdmin
            in
            ( { model | bookingsAdmin = newBookingAdmin }
            , cmd
            )

        SetDisplayMode dm ->
            if Auth.isLogged model.authPlugin.logInfo then
                let
                    ( newBookingAdmin, cmd ) =
                        BookingsAdmin.load
                            { logInfo = model.authPlugin.logInfo }
                            model.bookingsAdmin
                in
                ( { model
                    | displayMode = dm
                    , bookingsAdmin = newBookingAdmin
                  }
                , Cmd.batch
                    [ cmd ]
                )
            else
                ( model, Cmd.none )

        WinResize width height ->
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )

        SetZone zone ->
            ( { model | zone = zone }
            , Cmd.none
            )

        ChangeLang l ->
            ( { model | lang = l }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Gite Admin"
    , body =
        [ Element.layout
            [ width fill
            , Font.size 16
            ]
            (column
                [ width fill
                , height fill
                , paddingEach { sides | top = 3 }
                ]
                [ tabsView model
                , case model.displayMode of
                    DisplayAuth ->
                        Auth.view { zone = model.zone } model.authPlugin

                    DisplayFrontPageAdmin ->
                        GenericPageAdmin.view
                            { lang = model.lang
                            , width = model.width
                            , logInfo = Auth.getLogInfo model.authPlugin
                            }
                            model.frontPageAdmin

                    DisplayGalleryAdmin ->
                        GalleryAdmin.view
                            { lang = model.lang
                            , width = model.width
                            , logInfo = Auth.getLogInfo model.authPlugin
                            }
                            model.galleryAdmin

                    DisplayBookingsAdmin ->
                        BookingsAdmin.view
                            { lang = model.lang
                            , width = model.width
                            }
                            model.bookingsAdmin

                    DisplayAccessAdmin ->
                        GenericPageAdmin.view
                            { lang = model.lang
                            , width = model.width
                            , logInfo = Auth.getLogInfo model.authPlugin
                            }
                            model.accessPageAdmin

                    DisplayNearbyAdmin ->
                        GenericPageAdmin.view
                            { lang = model.lang
                            , width = model.width
                            , logInfo = Auth.getLogInfo model.authPlugin
                            }
                            model.nearbyPageAdmin

                    DisplayRatesAdmin ->
                        Element.none
                ]
            )
        ]
    }


tabsView : Model -> Element Msg
tabsView model =
    row
        [ Border.widthEach
            { top = 0
            , bottom = 2
            , left = 0
            , right = 0
            }
        , spacing 5
        , paddingEach
            { top = 0
            , bottom = 0
            , left = 5
            , right = 0
            }
        , width fill
        , Border.color
            (rgb 0.8 0.8 0.8)
        ]
        [ tabView model.displayMode
            DisplayFrontPageAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Front page Admin"
                    "Editeur page accueil"
                )
            )
        , tabView model.displayMode
            DisplayGalleryAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Our Gîte Admin"
                    "Editeur Notre gîte"
                )
            )
        , tabView model.displayMode
            DisplayRatesAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Rates Admin"
                    "Editeur tarifs"
                )
            )
        , tabView model.displayMode
            DisplayBookingsAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Bookings Admin"
                    "Editeur réservations"
                )
            )
        , tabView model.displayMode
            DisplayAccessAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Access page Admin"
                    "Editeur accès"
                )
            )
        , tabView model.displayMode
            DisplayNearbyAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Nearby page Admin"
                    "Editeur environs"
                )
            )
        , tabView model.displayMode
            DisplayAuth
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Authentication"
                    "Authentification"
                )
            )
        , column
            [ alignRight
            , alignTop
            , moveLeft 15
            , moveDown 3
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
        ]
