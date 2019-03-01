module Bookings.DatePicker.DatePicker
    exposing
        ( Availability(..)
        , Model
        , Msg
        , init
        , setCurrentDate
        , update
        , view
        )

import Bookings.DatePicker.Date exposing (..)
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
import Html.Events exposing (custom)
import Json.Decode as D
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Icons as Icons
import Style.Palette exposing (..)
import Task
import Time exposing (..)


type Msg
    = CurrentDate Date
    | NextMonth
    | PrevMonth
    | Pick Date
    | Open
    | Close
    | MouseEnter
    | MouseLeave
    | NoOp


type Availability
    = Available
    | NotAvailable
    | NoCheckIn
    | NoCheckOut
    | NotAvailableAdmin
    | NoCheckInAdmin
    | NoCheckOutAdmin
    | BookedAdmin String


defaultAvailability =
    always Available


type alias Model msg =
    { open : Bool
    , mouseInside : Bool
    , today : Date
    , currentDate : Date
    , currentDates : List Date
    , firstDayOfWeek : Time.Weekday
    , canPickDateInPast : Bool
    , outMsg : Msg -> msg
    }


placeholder : Lang -> String
placeholder l =
    case l of
        English ->
            "Please pick a date..."

        French ->
            "Choisissez une date..."


init : Maybe Date -> (Msg -> msg) -> ( Model msg, Cmd msg )
init mbStartDate outMsg =
    let
        startDate =
            Maybe.withDefault initDate mbStartDate
    in
    ( prepareDates startDate
        { open = False
        , mouseInside = False
        , today = startDate
        , currentDate = startDate
        , currentDates = []
        , firstDayOfWeek = Mon
        , canPickDateInPast = False
        , outMsg = outMsg
        }
    , [ Task.perform CurrentDate today
      ]
        |> Cmd.batch
        |> Cmd.map outMsg
    )


setCurrentDate : Date -> Model msg -> Model msg
setCurrentDate d model =
    prepareDates d <|
        { model | currentDate = d }


prepareDates : Date -> Model msg -> Model msg
prepareDates date model =
    -- set the currentDate and the 35/42 days to display in currentDates
    let
        start =
            firstOfMonth date |> subDays 6

        end =
            nextMonth date |> addDays 6
    in
    { model
        | currentDate = date
        , currentDates = datesInRange model.firstDayOfWeek start end
    }


update : Msg -> Model msg -> ( Model msg, Cmd msg, Maybe Date )
update msg model =
    case msg of
        CurrentDate date ->
            ( prepareDates
                date
                { model
                    | today = date
                }
            , Cmd.none
            , Nothing
            )

        NextMonth ->
            ( prepareDates (nextMonth model.currentDate) model
            , Cmd.none
            , Nothing
            )

        PrevMonth ->
            ( prepareDates (prevMonth model.currentDate) model
            , Cmd.none
            , Nothing
            )

        Pick date ->
            ( prepareDates
                date
                { model
                    | open = False
                }
            , Cmd.none
            , Just date
            )

        Open ->
            ( { model
                | open = True
              }
            , Cmd.none
            , Nothing
            )

        Close ->
            ( { model | open = False }
            , Cmd.none
            , Nothing
            )

        MouseEnter ->
            ( { model | mouseInside = True }
            , Cmd.none
            , Nothing
            )

        MouseLeave ->
            ( { model | mouseInside = False }
            , Cmd.none
            , Nothing
            )

        NoOp ->
            ( model
            , Cmd.none
            , Nothing
            )


type alias Config =
    { lang : Lang
    , availability : Date -> Availability
    , pickedDate : Maybe Date
    }


view : Config -> Model msg -> Element msg
view config model =
    let
        onPicker ev msg =
            htmlAttribute <|
                custom
                    ev
                    (D.succeed
                        { message = msg
                        , stopPropagation = True
                        , preventDefault = True
                        }
                    )
    in
    Element.map model.outMsg <|
        column
            [ width fill
            , if model.open then
                below <|
                    column
                        [ Border.color grey
                        , Border.rounded 3
                        , Border.width 1
                        , Background.color white
                        , moveUp 1
                        , Events.onMouseEnter MouseEnter
                        , Events.onMouseLeave MouseLeave
                        , onPicker "mousedown" NoOp
                        ]
                        [ monthSelectorView config model
                        , weekdaysView config model
                        , dayGrid config model
                        ]
              else
                noAttr
            ]
            [ pickedDateView config model
            ]


pickedDateView : Config -> Model msg -> Element Msg
pickedDateView config model =
    Input.text
        [ Background.color white
        , paddingXY 7 5
        , Border.rounded 3
        , Border.color grey
        , Border.width 1
        , width (px 250)
        , if model.open && not model.mouseInside then
            Events.onLoseFocus Close
          else
            noAttr
        , if model.open && not model.mouseInside then
            Events.onClick Close
          else
            Events.onClick Open
        , pointer
        , Font.size 16
        , htmlAttribute <| HtmlAttr.readonly True -- "readonly" "true"
        ]
        { onChange = always NoOp
        , text =
            Maybe.map (formatDate config.lang) config.pickedDate
                |> Maybe.withDefault ""
        , placeholder =
            Just <|
                Input.placeholder []
                    (text <| placeholder config.lang)
        , label = Input.labelHidden ""
        }


monthSelectorView : Config -> Model msg -> Element Msg
monthSelectorView config model =
    row
        [ width fill
        , paddingXY 7 5
        , Background.color lightGrey
        ]
        [ el
            [ Events.onClick PrevMonth
            , pointer
            , alignLeft
            , mouseOver
                [ alpha 0.8 ]
            , centerY
            ]
            (Icons.chevronLeft
                (Icons.defOptions
                    |> Icons.color grey
                )
            )
        , column
            [ centerX
            , spacing 10
            ]
            [ el
                ([ Font.size 16
                 , centerX
                 ]
                    ++ unselectable
                )
                (text <|
                    formatMonth
                        config.lang
                        (month model.currentDate)
                )
            , el
                ([ Font.size 12
                 , Font.bold
                 , centerX
                 ]
                    ++ unselectable
                )
                (text <| String.fromInt (year model.currentDate))
            ]
        , el
            [ Events.onClick NextMonth
            , pointer
            , alignRight
            , mouseOver
                [ alpha 0.8 ]
            , centerY
            ]
            (Icons.chevronRight
                (Icons.defOptions
                    |> Icons.color grey
                )
            )
        ]


weekdaysView : Config -> Model msg -> Element Msg
weekdaysView config model =
    row
        [ width fill
        , padding 5
        , Background.color lightGrey
        ]
        (List.map
            (\d ->
                el
                    ([ centerY
                     , width fill
                     , padding 5
                     , Font.size 16
                     ]
                        ++ unselectable
                    )
                    (text <| formatDay config.lang d)
            )
            [ model.firstDayOfWeek
            , addDows 1 model.firstDayOfWeek
            , addDows 2 model.firstDayOfWeek
            , addDows 3 model.firstDayOfWeek
            , addDows 4 model.firstDayOfWeek
            , addDows 5 model.firstDayOfWeek
            , addDows 6 model.firstDayOfWeek
            ]
        )


dayGrid : Config -> Model msg -> Element Msg
dayGrid config model =
    let
        days =
            groupDates model.currentDates

        isPickedDate d =
            case config.pickedDate of
                Nothing ->
                    False

                Just pd ->
                    Date.compare pd d == EQ

        availability =
            \d ->
                if
                    not model.canPickDateInPast
                        && (Date.compare d model.today
                                == LT
                           )
                        || (Date.compare d model.today
                                == EQ
                           )
                then
                    NotAvailable
                else
                    config.availability d

        dayColor d =
            case availability d of
                Available ->
                    lightGreen

                NotAvailable ->
                    lightRed

                NoCheckIn ->
                    orange

                NoCheckOut ->
                    orange

                NotAvailableAdmin ->
                    red

                NoCheckInAdmin ->
                    blue

                NoCheckOutAdmin ->
                    purple

                BookedAdmin s ->
                    orange

        handler d =
            case availability d of
                Available ->
                    Events.onClick (Pick d)

                NotAvailableAdmin ->
                    Events.onClick (Pick d)

                NoCheckInAdmin ->
                    Events.onClick (Pick d)

                NoCheckOutAdmin ->
                    Events.onClick (Pick d)

                BookedAdmin s ->
                    Events.onClick (Pick d)

                _ ->
                    noAttr

        dayView d =
            el
                ([ width fill
                 , centerY
                 , Font.center
                 , Font.color (dayColor d)
                 , handler d
                 , padding 3
                 , Font.size 16
                 , mouseOver
                    [ Background.color lightGrey ]
                 , if Date.compare d model.today == EQ then
                    Font.bold
                   else
                    noAttr
                 , if availability d == Available then
                    pointer
                   else
                    noAttr
                 ]
                    ++ (if isPickedDate d then
                            [ Font.color black
                            , Background.color lightGrey
                            ]
                        else if month d /= month model.currentDate then
                            [ alpha 0.4 ]
                        else
                            []
                       )
                    ++ unselectable
                )
                (text <| String.fromInt (day d))
    in
    column
        [ width fill ]
        (List.map
            (\daysRow ->
                row
                    [ width fill ]
                    (List.map dayView daysRow)
            )
            days
            |> List.intersperse
                (el
                    [ width fill
                    , Border.widthEach
                        { sides | top = 1 }
                    , Border.color lightGrey
                    ]
                    Element.none
                )
        )


groupDates : List Date -> List (List Date)
groupDates dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xs_ ->
                    if i == 6 then
                        go 0 xs_ [] (List.reverse (x :: racc) :: acc)
                    else
                        go (i + 1) xs_ (x :: racc) acc
    in
    go 0 dates [] []
