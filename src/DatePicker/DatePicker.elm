module DatePicker.DatePicker exposing (..)

import Date exposing (..)
import DatePicker.Date exposing (..)
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


type alias Settings =
    { placeholder : Lang -> String

    --, dateKind : Date -> DateKind
    , firstDayOfWeek : Time.Weekday
    , pickedDate : Maybe Date
    }


type alias Model msg =
    { open : Bool
    , today : Date
    , currentMonth : Date
    , currentDates : List Date
    , pickedDate : Maybe Date
    , settings : Settings
    , outMsg : Msg -> msg
    }


placeholder : Lang -> String
placeholder l =
    case l of
        English ->
            "Please pick a date..."

        French ->
            "Choisissez une date..."


defaultSettings : Settings
defaultSettings =
    { placeholder = placeholder

    --, dateKind = always Available
    , firstDayOfWeek = Mon
    , pickedDate = Nothing
    }


init : Settings -> (Msg -> msg) -> ( Model msg, Cmd msg )
init settings outMsg =
    let
        date =
            settings.pickedDate
                |> Maybe.withDefault initDate
    in
    ( prepareDates date
        { open = False
        , today = date
        , currentMonth = date
        , currentDates = []
        , pickedDate = settings.pickedDate
        , settings = settings
        , outMsg = outMsg
        }
    , Cmd.map outMsg <|
        Task.perform CurrentDate today
    )


prepareDates : Date -> Model msg -> Model msg
prepareDates date ({ settings } as model) =
    -- set the currentMonth and the 35/42 days to display in currentDates
    let
        start =
            firstOfMonth date |> subDays 6

        end =
            nextMonth date |> addDays 6
    in
    { model
        | currentMonth = date
        , currentDates = datesInRange settings.firstDayOfWeek start end
    }


update : Msg -> Model msg -> ( Model msg, Cmd msg, Maybe Date )
update msg model =
    case msg of
        CurrentDate date ->
            ( prepareDates
                (Maybe.withDefault date model.pickedDate)
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
                    | pickedDate = Just date
                    , open = False
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



--changeDateKind : (Date -> DateKind) -> Model msg -> Model msg
--changeDateKind f ({ settings } as model) =
--    let
--        newSettings =
--            { settings | dateKind = f }
--    in
--    { model | settings = newSettings }
--changeCurrentDate : Date -> Model msg -> ( Model msg, Cmd msg, Maybe Date )
--changeCurrentDate d model =
--    update (CurrentDate d) model


{-| Turn a list of dates into a list of date rows with 7 columns per
row representing each day of the week.
-}
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


type alias ViewConfig =
    { lang : Lang
    , availability : Date -> Availability
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            []
            [ monthSelectorView config model
            , weekdaysView config model
            , dayGrid config model
            ]


monthSelectorView : ViewConfig -> Model msg -> Element Msg
monthSelectorView config model =
    Element.none


weekdaysView : ViewConfig -> Model msg -> Element Msg
weekdaysView config model =
    Element.none


dayGrid : ViewConfig -> Model msg -> Element Msg
dayGrid config model =
    Element.none
