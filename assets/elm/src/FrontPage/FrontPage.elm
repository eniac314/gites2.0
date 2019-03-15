module FrontPage.FrontPage exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import FrontPage.FrontPageShared exposing (..)
import Http exposing (..)
import MultLang.MultLang exposing (..)


type alias Model msg =
    { outMsg : Msg -> msg
    , content : FrontPageContent
    }


type Msg
    = GotFrontPageContent (Result Http.Error FrontPageContent)
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { outMsg = outMsg
      , content = []
      }
    , Cmd.batch
        [ getFrontPageContent (outMsg << GotFrontPageContent)
        ]
    )


update : Msg -> Model msg -> Model msg
update msg model =
    case msg of
        GotFrontPageContent res ->
            case res of
                Ok content ->
                    { model
                        | content = content
                    }

                Err e ->
                    model

        NoOp ->
            model


view : { a | lang : Lang, width : Int } -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ width (maximum 1000 fill)
            , height (minimum 500 fill)
            , centerX
            , padding 15
            ]
            (List.map (frontPageItemView config) model.content)
