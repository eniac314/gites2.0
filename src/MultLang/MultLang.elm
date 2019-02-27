module MultLang.MultLang exposing (..)

import Element exposing (Element, text)


type Lang
    = English
    | French


type alias MultLangStr =
    { en : String
    , fr : String
    }


langKey : Lang -> Int
langKey l =
    case l of
        English ->
            0

        French ->
            1


textM : Lang -> MultLangStr -> Element msg
textM l ms =
    case l of
        English ->
            text (.en ms)

        French ->
            text (.fr ms)


strM : Lang -> MultLangStr -> String
strM l ms =
    case l of
        English ->
            .en ms

        French ->
            .fr ms
