port module LocalStorage.Cookies exposing
    ( CookiesPrefs
    , Model
    , Msg
    , cookiesPrefs
    , floatingDialogView
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation exposing (reload)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (buttonStyle)
import Style.Palette exposing (..)


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
    , prefSaved : Bool
    , outMsg : Msg -> msg
    }


type alias CookiesPrefs =
    { matomoConsent : Bool
    , googleConsent : Bool
    }


cookiesPrefs model =
    { matomoConsent = model.matomoConsent
    , googleConsent = model.googleConsent
    }


init outMsg =
    ( { matomoConsent = False
      , googleConsent = False
      , prefsSet = False
      , prefSaved = False
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
    | SavePrefs
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
                        , prefSaved = True
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | prefsSet = False
                      }
                    , Cmd.none
                    )

        SetMatomoConsent b ->
            ( { model
                | matomoConsent = b
                , prefSaved = False
              }
            , Cmd.none
            )

        SetGoogleConsent b ->
            ( { model
                | googleConsent = b
                , prefSaved = False
              }
            , Cmd.none
            )

        SavePrefs ->
            ( { model
                | prefSaved = True
                , prefsSet = True
              }
            , Cmd.batch
                [ setCookieConsent (encodeCookieConsent model)
                , if model.googleConsent then
                    loadExternalScript "https://www.google.com/recaptcha/api.js?render=explicit"

                  else
                    Cmd.none
                , if model.matomoConsent then
                    loadMatomo ()

                  else
                    Cmd.none
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
            ( { model | googleConsent = False, matomoConsent = False, prefsSet = False }
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
            [ paddingXY 15 10
            , Font.color white
            , Background.color charcoal
            , mouseOver
                [ Background.color lightGrey
                , Font.color black
                ]
            , pointer
            ]
    in
    if model.prefsSet then
        Element.none

    else
        Element.map model.outMsg <|
            el
                [ width (px config.width)
                , Background.color white
                , alignBottom
                ]
                (column
                    [ padding 15
                    , spacing 15
                    , centerX
                    ]
                    [ paragraph
                        []
                        [ el
                            []
                            (textM config.lang
                                (MultLangStr
                                    "This website needs cookies in order to work properly."
                                    "Ce site utilise des cookies pour fonctionner correctement."
                                )
                            )
                        , link
                            [ mouseOver
                                [ Font.color blue
                                ]
                            , Font.underline
                            , Font.color lightBlue
                            , paddingXY 10 0
                            ]
                            { url = "/cookies"
                            , label =
                                textM config.lang
                                    (MultLangStr
                                        "Learn more"
                                        "En savoir plus"
                                    )
                            }
                        ]
                    , row
                        [ spacing 15 ]
                        [ link []
                            { url = "/cookies"
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
                )


view : ViewConfig a -> Model msg -> Element msg
view config model =
    Element.map model.outMsg <|
        column
            [ spacing 40
            , padding 15
            , width (maximum 1000 fill)
            , centerX
            , Font.size 18
            , Font.family
                [ Font.typeface "times"
                ]
            ]
            [ el
                [ Font.bold
                , Font.size 22
                , Font.family
                    [ Font.typeface "Crimson Text"
                    , Font.sansSerif
                    ]
                ]
                (textM config.lang
                    (MultLangStr "Cookie policy"
                        "Politique d'utilisation des cookies"
                    )
                )
            , column
                [ spacing 30
                , padding 15
                , Border.rounded 20
                , Background.color white
                ]
                [ paragraph
                    [ Font.bold ]
                    [ textM config.lang
                        (MultLangStr
                            "This websise uses cookies for the following services:"
                            "Ce site web utilise des cookies pour les services suivants:"
                        )
                    ]
                , column
                    [ spacing 15 ]
                    [ paragraph
                        []
                        [ textM
                            config.lang
                            (MultLangStr
                                "- Services provided by Google: Google maps, Google recaptcha"
                                "- Services fournis par Google: google maps, google recaptcha pour protection des formulaires"
                            )
                        ]
                    , Input.checkbox
                        []
                        { onChange = SetGoogleConsent
                        , icon =
                            toggleCheckboxWidget
                                { offColor = grey
                                , onColor = green
                                , sliderColor = white
                                , toggleWidth = 40
                                , toggleHeight = 20
                                }
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
                    ]
                , column
                    [ spacing 15 ]
                    [ paragraph []
                        [ textM
                            config.lang
                            (MultLangStr
                                "Matomo analytics"
                                "- Matomo analytics pour obtenirs des statistiques de fréquentation. Ces données sont gérées en interne et ne sont partagées avec personne."
                            )
                        ]
                    , Input.checkbox
                        []
                        { onChange = SetMatomoConsent
                        , icon =
                            toggleCheckboxWidget
                                { offColor = grey
                                , onColor = green
                                , sliderColor = white
                                , toggleWidth = 40
                                , toggleHeight = 20
                                }
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
                    ]
                ]
            , paragraph []
                [ textM
                    config.lang
                    (MultLangStr
                        "Our website does not collect any data for advertising purposes"
                        "Notre site ne réalise aucune collecte de données à des fins publicitaires."
                    )
                ]
            , row
                [ spacing 15 ]
                [ Input.button
                    []
                    { onPress = Just ClearLocalStorage
                    , label =
                        el (buttonStyle True)
                            (textM config.lang
                                (MultLangStr
                                    "Clear settings"
                                    "Effacer les préférences pour ce site"
                                )
                            )
                    }
                , Input.button
                    []
                    { onPress =
                        if model.prefSaved then
                            Nothing

                        else
                            Just SavePrefs
                    , label =
                        el (buttonStyle (not model.prefSaved))
                            (textM config.lang
                                (MultLangStr
                                    "Save settings"
                                    "Sauvegarder préférences"
                                )
                            )
                    }
                ]
            ]


toggleCheckboxWidget : { offColor : Color, onColor : Color, sliderColor : Color, toggleWidth : Int, toggleHeight : Int } -> Bool -> Element msg
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleWidth, toggleHeight } checked =
    let
        pad =
            3

        sliderSize =
            toggleHeight - 2 * pad

        translation =
            (toggleWidth - sliderSize - pad)
                |> String.fromInt
    in
    el
        [ Background.color <|
            if checked then
                onColor

            else
                offColor
        , width <| px <| toggleWidth
        , height <| px <| toggleHeight
        , Border.rounded 14
        , inFront <|
            el [ height fill ] <|
                el
                    [ Background.color sliderColor
                    , Border.rounded <| sliderSize // 2
                    , width <| px <| sliderSize
                    , height <| px <| sliderSize
                    , centerY
                    , moveRight pad
                    , htmlAttribute <|
                        HA.style "transition" ".4s"
                    , htmlAttribute <|
                        if checked then
                            HA.style "transform" <| "translateX(" ++ translation ++ "px)"

                        else
                            HA.class ""
                    ]
                <|
                    text ""
        ]
    <|
        text ""
