module Bookings.DatePicker.Date
    exposing
        ( addDay
        , addDays
        , addDows
        , datesInRange
        , firstOfMonth
        , formatDate
        , formatDay
        , formatMonth
        , initDate
        , nextMonth
        , prevMonth
        , subDays
        )

import Date exposing (..)
import MultLang.MultLang exposing (..)
import Time exposing (..)


type alias Year =
    Int


type alias Day =
    Int


initDate : Date
initDate =
    fromCalendarDate 1992 May 29


formatDate : Lang -> Date -> String
formatDate l d =
    case l of
        English ->
            String.fromInt (year d)
                ++ "/"
                ++ (monthNumber d
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                   )
                ++ "/"
                ++ (String.fromInt (day d)
                        |> String.padLeft 2 '0'
                   )

        French ->
            (String.fromInt (day d)
                |> String.padLeft 2 '0'
            )
                ++ "/"
                ++ (monthNumber d
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                   )
                ++ "/"
                ++ String.fromInt (year d)


formatDay : Lang -> Time.Weekday -> String
formatDay l wd =
    case l of
        English ->
            case wd of
                Mon ->
                    "Mo"

                Tue ->
                    "Tu"

                Wed ->
                    "We"

                Thu ->
                    "Th"

                Fri ->
                    "Fr"

                Sat ->
                    "Sa"

                Sun ->
                    "Su"

        French ->
            case wd of
                Mon ->
                    "Lun"

                Tue ->
                    "Mar"

                Wed ->
                    "Mer"

                Thu ->
                    "Jeu"

                Fri ->
                    "Ven"

                Sat ->
                    "Sam"

                Sun ->
                    "Dim"


formatMonth : Lang -> Time.Month -> String
formatMonth l m =
    case l of
        English ->
            case m of
                Jan ->
                    "January"

                Feb ->
                    "February"

                Mar ->
                    "March"

                Apr ->
                    "April"

                May ->
                    "May"

                Jun ->
                    "June"

                Jul ->
                    "July"

                Aug ->
                    "August"

                Sep ->
                    "September"

                Oct ->
                    "October"

                Nov ->
                    "November"

                Dec ->
                    "December"

        French ->
            case m of
                Jan ->
                    "Janvier"

                Feb ->
                    "Février"

                Mar ->
                    "Mars"

                Apr ->
                    "Avril"

                May ->
                    "Mai"

                Jun ->
                    "Juin"

                Jul ->
                    "Juillet"

                Aug ->
                    "Août"

                Sep ->
                    "Septembre"

                Oct ->
                    "Octobre"

                Nov ->
                    "Novembre"

                Dec ->
                    "Décembre"


trimDates : Time.Weekday -> List Date -> List Date
trimDates firstDay dates =
    -- dates must be ordered
    let
        lastDay =
            predDow firstDay

        dl dates_ =
            case dates_ of
                [] ->
                    []

                x :: xs ->
                    if weekday x == firstDay then
                        dates_
                    else
                        dl xs

        dr dates_ =
            case dates_ of
                [] ->
                    []

                x :: xs ->
                    if weekday x == lastDay then
                        dates_
                    else
                        dr xs
    in
    dl dates
        |> List.reverse
        |> dr
        |> List.reverse


datesInRange : Time.Weekday -> Date -> Date -> List Date
datesInRange firstDay min max =
    let
        go x acc =
            let
                y =
                    subDay x
            in
            if isSameDate y min then
                y :: acc
            else
                go y (y :: acc)
    in
    go max []
        |> trimDates firstDay


predDow : Time.Weekday -> Time.Weekday
predDow d =
    let
        prev =
            (weekdayToNumber d - 1)
                |> modBy 7
    in
    if prev == 0 then
        Sun
    else
        numberToWeekday prev


subDows : Int -> Time.Weekday -> Time.Weekday
subDows =
    repeat succDow


succDow : Time.Weekday -> Time.Weekday
succDow d =
    weekdayToNumber d
        |> modBy 7
        |> (\r -> r + 1)
        |> numberToWeekday


addDows : Int -> Time.Weekday -> Time.Weekday
addDows =
    repeat succDow


isSameDate : Date -> Date -> Bool
isSameDate d1 d2 =
    Date.compare d1 d2 == EQ


firstOfMonth : Date -> Date
firstOfMonth d =
    fromCalendarDate (year d) (month d) 1


addDays : Int -> Date -> Date
addDays n d =
    add Days n d


addDay : Date -> Date
addDay d =
    add Days 1 d


subDays : Int -> Date -> Date
subDays n d =
    add Days (-1 * n) d


subDay : Date -> Date
subDay d =
    add Days -1 d


nextMonth : Date -> Date
nextMonth d =
    add Months 1 d
        |> firstOfMonth


prevMonth : Date -> Date
prevMonth d =
    add Months -1 d


repeat : (a -> a) -> Int -> a -> a
repeat f =
    let
        go n x =
            if n == 0 then
                x
            else
                go (n - 1) (f x)
    in
    go


dayToString : Int -> String
dayToString d =
    String.fromInt d
        |> String.padLeft 2 '0'


daysInMonth : Year -> Time.Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29
            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Year -> Bool
isLeapYear y =
    modBy 400 y == 0 || modBy 100 y /= 0 && modBy 4 y == 0
