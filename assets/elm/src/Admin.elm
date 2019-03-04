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
import FrontPage.FrontPageAdmin as FrontPageAdmin
import Http exposing (expectString)
import Internals.Helpers exposing (..)
import MultLang.MultLang exposing (..)
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
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        newFrontPageAdmin =
            FrontPageAdmin.init [] FrontPageAdminMsg

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
                newFrontPageAdmin =
                    FrontPageAdmin.update fpaMsg model.frontPageAdmin
            in
            ( { model | frontPageAdmin = newFrontPageAdmin }
            , Cmd.none
            )

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
                ]
                [ case model.displayMode of
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
