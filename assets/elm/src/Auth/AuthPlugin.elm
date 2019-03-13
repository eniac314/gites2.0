port module Auth.AuthPlugin exposing (LogInfo(..), Model, Msg, cmdIfLogged, getLogInfo, init, isLogged, secureGet, securePost, secureRequest, subscriptions, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import Http exposing (..)
import Internals.Helpers exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode exposing (..)
import Jwt
import Jwt.Decoders
import Jwt.Http
import Style.Helpers exposing (..)
import Task exposing (..)
import Time exposing (..)


port toAuthLocalStorage : Encode.Value -> Cmd msg


port fromAuthLocalStorage : (Decode.Value -> msg) -> Sub msg


secureGet :
    LogInfo
    ->
        { url : String
        , expect : Expect msg
        }
    -> Cmd msg
secureGet logInfo =
    case logInfo of
        LoggedIn { jwt } ->
            Jwt.Http.get jwt

        _ ->
            \_ -> Cmd.none


securePost :
    LogInfo
    ->
        { url : String
        , body : Body
        , expect : Expect msg
        }
    -> Cmd msg
securePost logInfo =
    case logInfo of
        LoggedIn { jwt } ->
            Jwt.Http.post jwt

        _ ->
            \_ -> Cmd.none


secureRequest :
    LogInfo
    ->
        { method : String
        , headers : List Header
        , url : String
        , body : Body
        , expect : Expect msg
        , timeout : Maybe Float
        , tracker : Maybe String
        }
    -> Cmd msg
secureRequest logInfo options =
    case logInfo of
        LoggedIn { jwt } ->
            Http.request
                { method = options.method
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ jwt) ]
                        ++ options.headers
                , url = options.url
                , body = options.body
                , expect = options.expect
                , timeout = options.timeout
                , tracker = options.tracker
                }

        _ ->
            Cmd.none


cmdIfLogged : LogInfo -> (String -> Cmd msg) -> Cmd msg
cmdIfLogged logInfo cmd =
    case logInfo of
        LoggedIn { jwt } ->
            cmd jwt

        _ ->
            Cmd.none


isLogged : LogInfo -> Bool
isLogged logInfo =
    case logInfo of
        LoggedIn { jwt } ->
            True

        _ ->
            False


type LogInfo
    = LoggedIn
        { username : String
        , jwt : String
        }
    | LoggedOut


type alias Model msg =
    { username : Maybe String
    , password : Maybe String
    , email : Maybe String
    , confirmPassword : Maybe String
    , logInfo : LogInfo
    , pluginMode : PluginMode
    , logs :
        List Log
    , checkForExistingJwtDone : Bool
    , externalMsg : Msg -> msg
    }


init externalMsg =
    ( { username = Nothing
      , password = Nothing
      , confirmPassword = Nothing
      , email = Nothing
      , logInfo = LoggedOut
      , pluginMode = LoginMode Initial
      , logs = []
      , checkForExistingJwtDone = False
      , externalMsg = externalMsg
      }
    , Cmd.map externalMsg <|
        Cmd.batch
            [ toAuthLocalStorage getJwt ]
    )


subscriptions model =
    Sub.map model.externalMsg <|
        Sub.batch
            [ fromAuthLocalStorage FromAuthLocalStorage
            , Time.every (60 * 1000) (\_ -> Ping)
            ]


reset model =
    ( { model
        | username = Nothing
        , password = Nothing
        , confirmPassword = Nothing
        , email = Nothing
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
    | Ping
    | Login
    | ConfirmLogin (Result Http.Error { username : String, jwt : String })
    | SignUp
    | ConfirmSignUp (Result Http.Error Bool)
    | Logout
    | ChangePluginMode PluginMode
    | AddLog Log
    | FromAuthLocalStorage Decode.Value
    | Quit
    | NoOp


update : Msg -> Model msg -> ( Model msg, Cmd msg, Maybe (PluginResult LogInfo) )
update msg model =
    let
        ( newModel, cmds, mbPluginResult ) =
            internalUpdate msg model
    in
    ( newModel, Cmd.map model.externalMsg cmds, mbPluginResult )


internalUpdate msg model =
    case msg of
        SetUsername s ->
            ( { model
                | username =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            , Nothing
            )

        SetPassword s ->
            ( { model
                | password =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            , Nothing
            )

        SetConfirmPassword s ->
            ( { model
                | confirmPassword =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            , Nothing
            )

        SetEmail s ->
            ( { model
                | email =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            , Nothing
            )

        Ping ->
            case model.logInfo of
                LoggedIn { jwt } ->
                    ( model
                    , refreshJwt jwt
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

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
                        , password = Nothing
                      }
                    , newLog
                        AddLog
                        "Echec connexion"
                        (Just <| httpErrorToString e)
                        True
                    , Nothing
                    )

                Ok { username, jwt } ->
                    ( { model
                        | logInfo =
                            LoggedIn
                                { username = username
                                , jwt = jwt
                                }
                        , password = Nothing
                        , pluginMode = LoginMode Success
                      }
                    , toAuthLocalStorage (setJwt username jwt)
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
                | pluginMode = LogoutMode Success
                , logInfo = LoggedOut
              }
            , toAuthLocalStorage clearJwt
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
            case Decode.decodeValue decodeLclStorResult res of
                Ok LclStorOk ->
                    ( { model | checkForExistingJwtDone = True }
                    , Cmd.none
                    , Nothing
                    )

                Ok LclStorErr ->
                    ( { model | checkForExistingJwtDone = True }
                    , Cmd.none
                    , Nothing
                    )

                Ok (LclStorJwt username jwt time) ->
                    if Jwt.isExpired time jwt == Ok False then
                        ( { model
                            | logInfo =
                                LoggedIn
                                    { username = username
                                    , jwt = jwt
                                    }
                            , pluginMode = LoginMode Success
                            , checkForExistingJwtDone = True
                          }
                        , Cmd.none
                        , Nothing
                        )
                    else
                        ( { model | checkForExistingJwtDone = True }
                        , toAuthLocalStorage clearJwt
                        , Nothing
                        )

                Err e ->
                    ( model, Cmd.none, Nothing )

        Quit ->
            ( model, Cmd.none, Just PluginQuit )

        NoOp ->
            ( model, Cmd.none, Nothing )



-------------------------------------------------------------------------------
-------------------
-- Coms and Json --
-------------------


login : Model msg -> Cmd Msg
login model =
    let
        body =
            Encode.object
                [ ( "login"
                  , Encode.object
                        [ ( "username"
                          , model.username
                                |> Maybe.withDefault ""
                                |> Encode.string
                          )
                        , ( "password"
                          , model.password
                                |> Maybe.withDefault ""
                                |> Encode.string
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


decodeLoginResult : Decode.Decoder { username : String, jwt : String }
decodeLoginResult =
    Decode.map2 (\a b -> { username = a, jwt = b })
        (Decode.field "username" Decode.string)
        (Decode.field "jwt" Decode.string)


signUp : Model msg -> Cmd Msg
signUp model =
    let
        body =
            Encode.object
                [ ( "new_user"
                  , Encode.object
                        [ ( "username"
                          , model.username
                                |> Maybe.withDefault ""
                                |> Encode.string
                          )
                        , ( "email"
                          , model.email
                                |> Maybe.withDefault ""
                                |> Encode.string
                          )
                        , ( "password"
                          , model.password
                                |> Maybe.withDefault ""
                                |> Encode.string
                          )
                        ]
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
    Decode.field "message" Decode.string
        |> Decode.map (\s -> s == "success")


type LclStorResult
    = LclStorOk
    | LclStorErr
    | LclStorJwt String String Posix


decodeLclStorResult : Decode.Decoder LclStorResult
decodeLclStorResult =
    Decode.oneOf
        [ Decode.map3 LclStorJwt
            (Decode.field "username" Decode.string)
            (Decode.field "jwt" Decode.string)
            (Decode.field "time"
                (Decode.int
                    |> Decode.map millisToPosix
                )
            )
        , Decode.field "result" Decode.string
            |> Decode.map
                (\res ->
                    if res == "ok" then
                        LclStorOk
                    else
                        LclStorErr
                )
        ]


getJwt =
    Encode.object
        [ ( "action", Encode.string "get" ) ]


setJwt username jwt =
    Encode.object
        [ ( "action", Encode.string "set" )
        , ( "payload"
          , Encode.object
                [ ( "jwt", Encode.string jwt )
                , ( "username", Encode.string username )
                ]
          )
        ]


clearJwt =
    Encode.object
        [ ( "action", Encode.string "clear" ) ]


refreshJwt jwt =
    Jwt.Http.get
        jwt
        { url = "/api/restricted/refresh_jwt"
        , expect = Http.expectJson ConfirmLogin decodeLoginResult
        }



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


view : { a | zone : Zone } -> Model msg -> Element msg
view config model =
    Element.map model.externalMsg <|
        case model.pluginMode of
            SignUpMode status ->
                signUpView config status model

            LoginMode status ->
                if not model.checkForExistingJwtDone then
                    Element.none
                else
                    loginView config status model

            LogoutMode status ->
                logoutView config status model


signUpView config status model =
    let
        canSignUp =
            (model.username /= Nothing)
                && (model.password /= Nothing)
                && (Maybe.map String.length model.password
                        |> Maybe.map (\l -> l >= 6 && l <= 20)
                        |> Maybe.withDefault False
                   )
                && (model.confirmPassword == model.password)
                && (model.email /= Nothing)

        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle_
                    { onChange =
                        SetUsername
                    , text =
                        model.username
                            |> Maybe.withDefault ""
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
                            |> Maybe.withDefault ""
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
                            |> Maybe.withDefault ""
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
                            |> Maybe.withDefault ""
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Confirmation: "))
                    , show = False
                    }
                , row
                    [ spacing 15 ]
                    [ Input.button (buttonStyle_ canSignUp)
                        { onPress =
                            if canSignUp then
                                Just SignUp
                            else
                                Nothing
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
        canLogin =
            (model.username /= Nothing)
                && (model.password /= Nothing)

        initialView =
            column
                [ spacing 15 ]
                [ Input.text textInputStyle_
                    { onChange =
                        SetUsername
                    , text =
                        model.username
                            |> Maybe.withDefault ""
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
                            |> Maybe.withDefault ""
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft [ centerY ]
                            (el [ width (px 110) ] (text "Mot de passe: "))
                    , show = False
                    }
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ canLogin)
                        { onPress =
                            if canLogin then
                                Just Login
                            else
                                Nothing
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
                , row [ spacing 15 ]
                    [ Input.button (buttonStyle_ True)
                        { onPress = Just <| ChangePluginMode (LogoutMode Initial)
                        , label = text "Deconnexion"
                        }
                    , Input.button (buttonStyle_ True)
                        { onPress = Just <| Quit
                        , label = text "Admin"
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
