module Internals.DropdownSelect exposing (..)

import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html as Html
import Html.Attributes as HtmlAttr
import Json.Decode as D
import Style.Helpers exposing (noAttr)
import Style.Palette exposing (..)


type alias Model =
    { open : Bool
    , mouseInside : Bool
    }


init =
    Model False False


type Msg
    = Open
    | Close
    | MouseEnter
    | MouseLeave
    | NoOp


type alias Config msg =
    { outMsg : Msg -> msg
    , items : List ( String, msg )
    , selected : Maybe String
    , placeholder : Maybe String
    , label : Maybe String
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Open ->
            { model | open = True }

        Close ->
            { model | open = False }

        MouseEnter ->
            { model | mouseInside = True }

        MouseLeave ->
            { model | mouseInside = False }

        NoOp ->
            model


view : Config msg -> Model -> Element msg
view config model =
    let
        om =
            config.outMsg
    in
    Input.text
        [ if model.open && not model.mouseInside then
            Events.onLoseFocus (om Close)
          else
            noAttr
        , if model.open then
            below <|
                column
                    [ Background.color white
                    , Border.color lightGrey
                    , Border.width 1
                    , Events.onMouseEnter (om MouseEnter)
                    , Events.onMouseLeave (om MouseLeave)
                    , width (minimum 200 fill)
                    ]
                    (List.map (itemView config) config.items)
          else
            noAttr
        , if model.open then
            Events.onClick (om Close)
          else
            Events.onClick (om Open)
        , Font.size 16
        , width (px 200)
        , paddingXY 10 5
        ]
        { onChange = always (om NoOp)
        , text =
            config.selected
                |> Maybe.withDefault ""
        , placeholder =
            Maybe.map
                (\s ->
                    Input.placeholder [] (text s)
                )
                config.placeholder
        , label =
            Input.labelHidden ""
        }


itemView : Config msg -> ( String, msg ) -> Element msg
itemView config ( name, handler ) =
    let
        om =
            config.outMsg
    in
    el
        [ width fill
        , Events.onClick handler
        , padding 10
        , mouseOver
            [ Background.color lightGrey
            ]
        , pointer
        ]
        (text name)
