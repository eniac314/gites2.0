port module LocalStorage.Cookies exposing
    ( CookiesPrefs
    , Model
    , Msg
    , floatingDialogView
    , init
    , subscriptions
    , update
    , view
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)


port loadExternalScript : String -> Cmd msg


port loadMatomo : () -> Cmd msg


port setCookieConsent : E.Value -> Cmd msg


port loadLocalPrefs : () -> Cmd msg


port clearLocalStorage : () -> Cmd msg


port localPrefs : (D.Value -> msg) -> Sub msg


type alias Model msg =
    { matomoConsent : Bool
    , googleConsent : Bool
    , prefsSet : Bool
    , outMsg : Msg -> msg
    }


type alias CookiesPrefs =
    { matomoConsent : Bool
    , googleConsent : Bool
    }


init outMsg =
    ( { matomoConsent = False
      , googleConsent = False
      , prefsSet = False
      , outMsg = outMsg
      }
    , loadLocalPrefs ()
    )


subscriptions model =
    Sub.map model.outMsg <|
        localPrefs LocalPrefsLoaded


type Msg
    = LocalPrefsLoaded D.Value
    | SetMatomoConsent Bool
    | SetGoogleConsent Bool
    | AcceptAll
    | ClearLocalStorage
    | NoOp


encodeCookieConsent : Model msg -> E.Value
encodeCookieConsent model =
    E.object
        [ ( "allowMatomoTracking", E.bool model.matomoConsent )
        , ( "allowGoogleServices", E.bool model.googleConsent )
        ]


decodePrefs : D.Decoder { matomoConsent : Bool, googleConsent : Bool }
decodePrefs =
    D.map2 (\mC gC -> { matomoConsent = mC, googleConsent = gC })
        (D.field "allowMatomoTracking" D.bool)
        (D.field "allowGoogleServices" D.bool)


update msg model =
    case msg of
        LocalPrefsLoaded prefsVal ->
            case D.decodeValue decodePrefs prefsVal of
                Ok { matomoConsent, googleConsent } ->
                    ( { model
                        | googleConsent = googleConsent
                        , matomoConsent = matomoConsent
                        , prefsSet = True
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | prefsSet = True
                      }
                    , Cmd.none
                    )

        SetMatomoConsent b ->
            let
                newModel =
                    { model | matomoConsent = b }
            in
            ( newModel
            , Cmd.batch
                [ setCookieConsent (encodeCookieConsent newModel)
                , loadMatomo ()
                ]
                |> Cmd.map model.outMsg
            )

        SetGoogleConsent b ->
            let
                newModel =
                    { model | googleConsent = b }
            in
            ( newModel
            , Cmd.batch
                [ setCookieConsent (encodeCookieConsent newModel)
                , loadExternalScript "https://www.google.com/recaptcha/api.js?render=explicit"
                ]
                |> Cmd.map model.outMsg
            )

        AcceptAll ->
            let
                newModel =
                    { model
                        | googleConsent = True
                        , matomoConsent = True
                        , prefsSet = True
                    }
            in
            ( newModel
            , Cmd.batch
                [ setCookieConsent (encodeCookieConsent newModel)
                , loadExternalScript "https://www.google.com/recaptcha/api.js?render=explicit"
                , loadMatomo ()
                ]
                |> Cmd.map model.outMsg
            )

        ClearLocalStorage ->
            ( { model | googleConsent = False, matomoConsent = False }
            , clearLocalStorage ()
                |> Cmd.map model.outMsg
            )

        NoOp ->
            ( model, Cmd.none )


type alias ViewConfig a =
    { a
        | lang : Lang
        , width : Int
    }


floatingDialogView : ViewConfig a -> Model msg -> Element msg
floatingDialogView config model =
    let
        controlStyle =
            []
    in
    if model.prefsSet then
        Element.none

    else
        Element.map model.outMsg <|
            column
                [ padding 15
                , width fill
                ]
                [ el
                    [ centerX ]
                    (textM config.lang
                        (MultLangStr
                            "This website needs cookies in order to work properly"
                            "Ce site utilise des cookies pour fonctionner correctement"
                        )
                    )
                , row
                    []
                    [ link []
                        { url = ""
                        , label =
                            el controlStyle
                                (textM config.lang
                                    (MultLangStr
                                        "Manage cookies"
                                        "Gérer les cookies"
                                    )
                                )
                        }
                    , Input.button
                        []
                        { onPress = Just AcceptAll
                        , label =
                            el controlStyle
                                (textM config.lang
                                    (MultLangStr
                                        "Accept"
                                        "Accepter"
                                    )
                                )
                        }
                    ]
                ]


view : ViewConfig a -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ spacing 15
            , padding 15
            , width (maximum 1000 fill)
            , centerX
            , Font.size 18
            , Font.family
                [ Font.typeface "times"
                ]
            ]
            [ Input.checkbox
                []
                { onChange = SetGoogleConsent
                , icon = Input.defaultCheckbox
                , checked = model.googleConsent
                , label =
                    Input.labelRight []
                        (textM config.lang
                            (MultLangStr
                                "Allow google services using cookies"
                                "Autoriser les services Google nécéssitants des cookies"
                            )
                        )
                }
            , Input.checkbox
                []
                { onChange = SetMatomoConsent
                , icon = Input.defaultCheckbox
                , checked = model.matomoConsent
                , label =
                    Input.labelRight []
                        (textM config.lang
                            (MultLangStr
                                "Allow Matomo analytics"
                                "Autoriser le service d'analyse des visites Matomo"
                            )
                        )
                }
            , Input.button
                []
                { onPress = Just ClearLocalStorage
                , label =
                    el []
                        (textM config.lang
                            (MultLangStr
                                "Clear all cookies for this website"
                                "Effacer les cookies pour ce site"
                            )
                        )
                }
            ]
