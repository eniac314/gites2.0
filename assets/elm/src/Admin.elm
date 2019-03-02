module Admin exposing (..)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FrontPage.FrontPageAdmin as FrontPageAdmin
import MultLang.MultLang exposing (..)
import Browser.Events exposing (onResize)


--import Auth.AuthPlugin as Auth


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
    , frontPageAdmin : FrontPageAdmin.Model Msg
    }


type Msg
    = FrontPageAdminMsg FrontPageAdmin.Msg
    | WinResize Int Int
    | NoOp


type DisplayMode
    = DisplayFrontPageAdmin
    | DisplayBookingsAdmin
    | DisplayNearbyAdmin
    | DisplayRatesAdmin


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize WinResize ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        newFrontPageAdmin =
            FrontPageAdmin.init [] FrontPageAdminMsg
    in
        ( { displayMode = DisplayFrontPageAdmin
          , lang = English
          , width =
                flags.width
          , height =
                flags.height
          , currentTime =
                flags.currentTime
          , frontPageAdmin = newFrontPageAdmin
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    DisplayFrontPageAdmin ->
                        FrontPageAdmin.view
                            { lang = model.lang
                            , width = model.width
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
