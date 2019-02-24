module Bookings.Bookings exposing (..)

import Bookings.DatePicker.DatePicker as DatePicker
import Browser exposing (element)
import Date exposing (..)
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
import MultLang.MultLang exposing (..)
import Style.Palette exposing (..)


main : Program () (Model Msg) Msg
main =
    Browser.element
        { init = \_ -> init identity
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch []


type alias Model msg =
    { checkInPicker : DatePicker.Model msg
    , checkInDate : Maybe Date
    , outMsg : Msg -> msg
    }


type Msg
    = CheckInPickerMsg DatePicker.Msg
    | NoOp


init outMsg =
    let
        ( checkInPicker, checkInPickerCmd ) =
            DatePicker.init (outMsg << CheckInPickerMsg)
    in
    ( { checkInPicker = checkInPicker
      , checkInDate = Nothing
      , outMsg = outMsg
      }
    , Cmd.batch
        [ checkInPickerCmd ]
    )


update msg model =
    case msg of
        CheckInPickerMsg pickerMsg ->
            let
                ( newCheckInPicker, cmd, mbDate ) =
                    DatePicker.update pickerMsg model.checkInPicker
            in
            ( { model
                | checkInPicker = newCheckInPicker
                , checkInDate = mbDate
              }
            , cmd
            )

        NoOp ->
            ( model, Cmd.none )


view model =
    Element.layout [] <|
        column
            []
            [ DatePicker.view
                { lang = English
                , availability = always DatePicker.Available
                , pickedDate = model.checkInDate
                }
                model.checkInPicker
            ]
