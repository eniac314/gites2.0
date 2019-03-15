module Admin exposing (..)

import Auth.AuthPlugin as Auth
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
import FrontPage.FrontPageAdmin as FrontPageAdmin
import Http exposing (expectString)
import Internals.Helpers exposing (..)
import Internals.Uploader as Uploader
import MultLang.MultLang exposing (..)
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
    }


type alias Model =
    { displayMode : DisplayMode
    , lang : Lang
    , width : Int
    , height : Int
    , currentTime : Int
    , zone : Time.Zone
    , authPlugin : Auth.Model Msg
    , frontPageAdmin : FrontPageAdmin.Model Msg
    }


type Msg
    = AuthMsg Auth.Msg
    | FrontPageAdminMsg FrontPageAdmin.Msg
    | SetDisplayMode DisplayMode
    | WinResize Int Int
    | SetZone Time.Zone
    | NoOp


type DisplayMode
    = DisplayAuth
    | DisplayFrontPageAdmin
    | DisplayBookingsAdmin
    | DisplayNearbyAdmin
    | DisplayRatesAdmin


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize WinResize
        , Auth.subscriptions model.authPlugin
        , FrontPageAdmin.subscriptions model.frontPageAdmin
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( newFrontPageAdmin, fpaCmd ) =
            FrontPageAdmin.init FrontPageAdminMsg

        ( newAuthPlugin, authPluginCmd ) =
            Auth.init AuthMsg
    in
    ( { displayMode = DisplayAuth
      , lang = English
      , width =
            flags.width
      , height =
            flags.height
      , currentTime =
            flags.currentTime
      , zone = Time.utc
      , authPlugin = newAuthPlugin
      , frontPageAdmin = newFrontPageAdmin
      }
    , Cmd.batch
        [ Task.perform SetZone Time.here
        , authPluginCmd
        , fpaCmd
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
                    FrontPageAdmin.update
                        { logInfo = model.authPlugin.logInfo }
                        fpaMsg
                        model.frontPageAdmin
            in
            ( { model | frontPageAdmin = newFrontPageAdmin }
            , cmd
            )

        SetDisplayMode dm ->
            if Auth.isLogged model.authPlugin.logInfo then
                ( { model
                    | displayMode = dm
                  }
                , Cmd.none
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
                        FrontPageAdmin.view
                            { lang = model.lang
                            , width = model.width
                            , logInfo = Auth.getLogInfo model.authPlugin
                            }
                            model.frontPageAdmin

                    DisplayBookingsAdmin ->
                        Element.none

                    DisplayNearbyAdmin ->
                        Element.none

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
            DisplayBookingsAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Bookings Admin"
                    "Editeur réservations"
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
            DisplayRatesAdmin
            SetDisplayMode
            (strM model.lang
                (MultLangStr "Rates Admin"
                    "Editeur tarifs"
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
        ]
