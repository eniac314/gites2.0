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
    ()


type alias Model =
    { displayMode : DisplayMode
    , lang : Lang
    , frontPageAdmin : FrontPageAdmin.Model Msg
    }


type Msg
    = FrontPageAdminMsg FrontPageAdmin.Msg
    | NoOp


type DisplayMode
    = DisplayFrontPageAdmin
    | DisplayBookingsAdmin
    | DisplayNearbyAdmin
    | DisplayRatesAdmin


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        newFrontPageAdmin =
            FrontPageAdmin.init [] FrontPageAdminMsg
    in
        ( { displayMode = DisplayFrontPageAdmin
          , lang = English
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
                            { lang = model.lang }
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
