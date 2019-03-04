port module Auth.AuthPlugin exposing (LogInfo(..), Model, Msg, cmdIfLogged, getLogInfo, init, subscriptions, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import Http exposing (..)
import Jwt
import Jwt.Decoders
import Jwt.Http
import Style.Helpers exposing (..)
import Internals.Helpers exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode exposing (..)
import Task exposing (..)
import Time exposing (..)


port toAuthLocalStorage : Encode.Value -> Cmd msg


port fromAuthLocalStorage : (Decode.Value -> msg) -> Sub msg


cmdIfLogged : LogInfo -> (String -> Cmd msg) -> Cmd msg
cmdIfLogged logInfo cmd =
    case logInfo of
        LoggedIn { jwt } ->
            cmd jwt

        _ ->
            Cmd.none


type LogInfo
    = LoggedIn
        { username : String
        , jwt : String
        }
    | LoggedOut


type alias Model msg =
    { username : String
    , password : String
    , email : String
    , confirmPassword : String
    , logInfo : LogInfo
    , pluginMode : PluginMode
    , logs :
        List Log
    , externalMsg : Msg -> msg
    }


init externalMsg =
    ( { username = ""
      , password = ""
      , confirmPassword = ""
      , email = ""
      , logInfo = LoggedOut
      , pluginMode = LoginMode Initial
      , logs = []
      , externalMsg = externalMsg
      }
    , Cmd.map externalMsg <|
        Cmd.batch
            [ toAuthLocalStorage getJwt ]
    )


getJwt =
    Encode.object
        [ ( "cmd", Encode.string "get" ) ]


setJwt jwt =
    Encode.object
        [ ( "cmd", Encode.string "set" )
        , ( "payload", Encode.string jwt )
        ]


subscriptions model =
    Sub.map model.externalMsg <|
        Sub.batch
            [ fromAuthLocalStorage FromAuthLocalStorage
            ]


reset model =
    ( { model
        | username = ""
        , password = ""
        , confirmPassword = ""
        , email = ""
        , pluginMode = LoginMode Initial
        , logs = []
      }
    , login model
    )


getLogInfo model =
    model.logInfo


type PluginMode
    = SignUpMode Status
    | LoginMode Status
    | LogoutMode Status


type Msg
    = SetUsername String
    | SetPassword String
    | SetConfirmPassword String
    | SetEmail String
    | Login
    | ConfirmLogin (Result Http.Error ( LogInfo, String ))
    | SignUp
    | ConfirmSignUp (Result Http.Error Bool)
    | Logout
    | ConfirmLogout (Result Http.Error Bool)
    | ChangePluginMode PluginMode
    | AddLog Log
    | FromAuthLocalStorage Decode.Value
    | Quit
    | NoOp


update msg model =
    let
        ( newModel, cmds, mbPluginResult ) =
            internalUpdate msg model
    in
        ( newModel, Cmd.map model.externalMsg cmds, mbPluginResult )


internalUpdate msg model =
    case msg of
        SetUsername s ->
            ( { model | username = s }
            , Cmd.none
            , Nothing
            )

        SetPassword s ->
            ( { model | password = s }
            , Cmd.none
            , Nothing
            )

        SetConfirmPassword s ->
            ( { model | confirmPassword = s }
            , Cmd.none
            , Nothing
            )

        SetEmail s ->
            ( { model | email = s }
            , Cmd.none
            , Nothing
            )

        Login ->
            ( { model
                | pluginMode = LoginMode Waiting
              }
            , login model
            , Nothing
            )

        ConfirmLogin res ->
            case res of
                Err e ->
                    ( { model
                        | logInfo = LoggedOut
                        , pluginMode = LoginMode Failure
                      }
                    , newLog
                        AddLog
                        "Echec connexion"
                        (Just <| httpErrorToString e)
                        True
                    , Nothing
                    )

                Ok ( logInfo, jwt ) ->
                    ( { model
                        | logInfo = logInfo
                        , pluginMode = LoginMode Success
                      }
                    , toAuthLocalStorage (setJwt jwt)
                    , Nothing
                    )

        SignUp ->
            ( { model
                | pluginMode = SignUpMode Waiting
              }
            , signUp model
            , Nothing
            )

        ConfirmSignUp res ->
            case res of
                Err e ->
                    ( { model | pluginMode = SignUpMode Failure }
                    , newLog
                        AddLog
                        "Echec création compte"
                        (Just <| httpErrorToString e)
                        True
                    , Nothing
                    )

                Ok _ ->
                    ( { model | pluginMode = SignUpMode Success }
                    , Cmd.none
                    , Nothing
                    )

        Logout ->
            ( { model
                | pluginMode = LogoutMode Waiting
              }
            , logout
            , Nothing
            )

        ConfirmLogout res ->
            case res of
                Err e ->
                    ( { model | pluginMode = LogoutMode Failure }
                    , newLog
                        AddLog
                        "Echec déconnexion"
                        (Just <| httpErrorToString e)
                        True
                    , Nothing
                    )

                Ok _ ->
                    ( { model
                        | pluginMode = LogoutMode Success
                        , logInfo = LoggedOut
                      }
                    , Cmd.none
                    , Nothing
                    )

        ChangePluginMode mode ->
            ( { model
                | pluginMode = mode
              }
            , Cmd.none
            , Nothing
            )

        AddLog log ->
            ( { model | logs = log :: model.logs }
            , Cmd.none
            , Nothing
            )

        FromAuthLocalStorage res ->
            ( model, Cmd.none, Nothing )

        Quit ->
            ( model, Cmd.none, Just PluginQuit )

        NoOp ->
            ( model, Cmd.none, Nothing )


login : Model msg -> Cmd Msg
login model =
    let
        body =
            Encode.object
                [ ( "login"
                  , Encode.object
                        [ ( "username"
                          , Encode.string (.username model)
                          )
                        , ( "password"
                          , Encode.string (.password model)
                          )
                        ]
                  )
                ]
                |> Http.jsonBody
    in
        Http.post
            { url = "/api/login"
            , body = body
            , expect = Http.expectJson ConfirmLogin decodeLoginResult
            }



--decodeLocalStorageResult


decodeLoginResult : Decode.Decoder ( LogInfo, String )
decodeLoginResult =
    Decode.map2 (\a b -> ( LoggedIn { username = a, jwt = b }, b ))
        (Decode.field "username" Decode.string)
        (Decode.field "jwt" Decode.string)


signUp : Model msg -> Cmd Msg
signUp model =
    let
        body =
            Encode.object
                [ ( "username"
                  , Encode.string (.username model)
                  )
                , ( "password"
                  , Encode.string (.password model)
                  )
                ]
                |> Http.jsonBody
    in
        Http.post
            { url = "/api/signup"
            , body = body
            , expect = Http.expectJson ConfirmSignUp decodeSignupResult
            }


decodeSignupResult =
    Decode.field "signUpComplete" Decode.bool


logout : Cmd Msg
logout =
    Http.get
        { url = "logout.php"
        , expect = Http.expectJson ConfirmLogout decodeLogoutResult
        }


decodeLogoutResult =
    Decode.field "notLoggedIn" Decode.bool


view config model =
    Element.map model.externalMsg <|
        case model.pluginMode of
            SignUpMode status ->
                signUpView config status model

            LoginMode status ->
                loginView config status model

            LogoutMode status ->
                logoutView config status model


signUpView config status model =
    let
        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle_
                    { onChange =
                        SetUsername
                    , text =
                        model.username
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Identifiant: "))
                    }
                , Input.text textInputStyle_
                    { onChange =
                        SetEmail
                    , text =
                        model.email
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Email: "))
                    }
                , Input.newPassword textInputStyle_
                    { onChange =
                        SetPassword
                    , text =
                        model.password
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Mot de passe: "))
                    , show = False
                    }
                , Input.newPassword textInputStyle_
                    { onChange =
                        SetConfirmPassword
                    , text =
                        model.confirmPassword
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Confirmation: "))
                    , show = False
                    }
                , row
                    [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress = Just SignUp
                        , label = text "Envoyer"
                        }
                    , Input.button (buttonStyle_ True)
                        { onPress = Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Retour"
                        }
                    ]
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Traitement en cours, veuillez patienter" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Inscription réussie!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress =
                            Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Connexion"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec inscription!"
                , logsView model.logs config.zone
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress =
                            Just <| ChangePluginMode (SignUpMode Initial)
                        , label = text "Réessayer"
                        }
                    , Input.button (buttonStyle_ True)
                        { onPress = Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Retour"
                        }
                    ]
                ]
    in
        column
            [ padding 15
            , spacing 15
            , Font.size 16
            , alignTop
            ]
            [ text "Nouvel utilisateur: "
            , case status of
                Initial ->
                    initialView

                Waiting ->
                    waitingView

                Success ->
                    successView

                Failure ->
                    failureView
            ]


loginView config status model =
    let
        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle_
                    { onChange =
                        SetUsername
                    , text =
                        model.username
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Identifiant: "))
                    }
                , Input.currentPassword textInputStyle_
                    { onChange =
                        SetPassword
                    , text =
                        model.password
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Mot de passe: "))
                    , show = False
                    }
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress = Just Login
                        , label = text "Connexion"
                        }
                    , Input.button (buttonStyle_ True)
                        { onPress = Just <| ChangePluginMode (SignUpMode Initial)
                        , label = text "Nouvel utilisateur"
                        }
                    ]
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Traitement en cours, veuillez patienter" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Connexion réussie!"
                , case model.logInfo of
                    LoggedIn { jwt } ->
                        Jwt.getTokenHeader jwt
                            |> Result.withDefault ""
                            |> text

                    _ ->
                        Element.none
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress = Just <| ChangePluginMode (LogoutMode Initial)
                        , label = text "Deconnexion"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec Connexion!"
                , logsView model.logs config.zone
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress =
                            Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Réessayer"
                        }
                    ]
                ]
    in
        column
            [ padding 15
            , spacing 15
            , Font.size 16
            , alignTop
            ]
            [ text "Connexion: "
            , case status of
                Initial ->
                    initialView

                Waiting ->
                    waitingView

                Success ->
                    successView

                Failure ->
                    failureView
            ]


logoutView config status model =
    let
        initialView =
            column
                [ spacing 15 ]
                [ Input.button (buttonStyle_ True)
                    { onPress = Just Logout
                    , label = text "Se déconnecter"
                    }
                ]

        waitingView =
            column
                [ spacing 15 ]
                [ text "Traitement en cours, veuillez patienter" ]

        successView =
            column
                [ spacing 15 ]
                [ text "Déconnexion réussie!"
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress = Just <| ChangePluginMode (LoginMode Initial)
                        , label = text "Connexion"
                        }
                    ]
                ]

        failureView =
            column
                [ spacing 15 ]
                [ text "Echec déconnexion!"
                , logsView model.logs config.zone
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress =
                            Just <| ChangePluginMode (LogoutMode Initial)
                        , label = text "Réessayer"
                        }
                    ]
                ]
    in
        column
            [ padding 15
            , spacing 15
            , Font.size 16
            , alignTop
            ]
            [ text "Déconnexion: "
            , case status of
                Initial ->
                    initialView

                Waiting ->
                    waitingView

                Success ->
                    successView

                Failure ->
                    failureView
            ]
