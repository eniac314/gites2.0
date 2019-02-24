module Bookings.DatePicker.DatePicker exposing (..)

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
      --| Change String
    | Open
    | Close


type Availability
    = Available
    | NotAvailable
    | NoArrival
    | NoDeparture
    | NotAvailableAdmin
    | NoArrivalAdmin
    | NoDepartureAdmin
    | BookedAdmin String


defaultAvailability =
    always Available


type alias Model msg =
    { open : Bool
    , today : Date
    , currentMonth : Date
    , currentDates : List Date
    , firstDayOfWeek : Time.Weekday
    , outMsg : Msg -> msg
    }


placeholder : Lang -> String
placeholder l =
    case l of
        English ->
            "Please pick a date..."

        French ->
            "Choisissez une date..."


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( prepareDates initDate
        { open = False
        , today = initDate
        , currentMonth = initDate
        , currentDates = []
        , firstDayOfWeek = Mon
        , outMsg = outMsg
        }
    , Cmd.map outMsg <|
        Task.perform CurrentDate today
    )


prepareDates : Date -> Model msg -> Model msg
prepareDates date model =
    -- set the currentMonth and the 35/42 days to display in currentDates
    let
        start =
            firstOfMonth date |> subDays 6

        end =
            nextMonth date |> addDays 6
    in
    { model
        | currentMonth = date
        , currentDates = datesInRange model.firstDayOfWeek start end
    }


update : Msg -> Model msg -> ( Model msg, Cmd msg, Maybe Date )
update msg model =
    case msg of
        CurrentDate date ->
            ( prepareDates
                date
                { model | today = date }
            , Cmd.none
            , Nothing
            )

        NextMonth ->
            ( prepareDates (nextMonth model.currentMonth) model
            , Cmd.none
            , Nothing
            )

        PrevMonth ->
            ( prepareDates (prevMonth model.currentMonth) model
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
            ( { model | open = True }
            , Cmd.none
            , Nothing
            )

        Close ->
            ( { model | open = False }
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
    Element.map model.outMsg <|
        column
            [ width fill ]
            [ pickedDateView config model
            , el
                [ width fill
                , height fill
                , if model.open then
                    Events.onClick Close
                  else
                    noAttr
                ]
                Element.none
            , el
                [ inFront <|
                    column
                        [ Border.color grey
                        , Border.width 1
                        ]
                        (if model.open then
                            [ monthSelectorView config model
                            , weekdaysView config model
                            , dayGrid config model
                            ]
                         else
                            []
                        )
                ]
                Element.none
            ]


pickedDateView : Config -> Model msg -> Element Msg
pickedDateView config model =
    el
        [ Background.color white
        , paddingXY 7 5
        , Border.rounded 3
        , Border.color grey
        , Border.width 1
        , width (px 250)
        , if model.open then
            Events.onClick Close
          else
            Events.onClick Open
        ]
        (Maybe.map (formatDate config.lang) config.pickedDate
            |> Maybe.withDefault (placeholder config.lang)
            |> text
        )


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
                        (month model.currentMonth)
                )
            , el
                ([ Font.size 12
                 , Font.bold
                 , centerX
                 ]
                    ++ unselectable
                )
                (text <| String.fromInt (year model.currentMonth))
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

        dayColor d =
            case config.availability d of
                Available ->
                    green

                NotAvailable ->
                    orange

                NoArrival ->
                    orange

                NoDeparture ->
                    purple

                NotAvailableAdmin ->
                    red

                NoArrivalAdmin ->
                    blue

                NoDepartureAdmin ->
                    purple

                BookedAdmin s ->
                    orange

        handler d =
            case config.availability d of
                Available ->
                    Events.onClick (Pick d)

                NotAvailableAdmin ->
                    Events.onClick (Pick d)

                NoArrivalAdmin ->
                    Events.onClick (Pick d)

                NoDepartureAdmin ->
                    Events.onClick (Pick d)

                BookedAdmin s ->
                    Events.onClick (Pick d)

                _ ->
                    noAttr

        dayView d =
            el
                [ width fill
                , centerY
                , Font.center
                , Font.color (dayColor d)
                , handler d
                , padding 3
                , Font.size 16
                ]
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
