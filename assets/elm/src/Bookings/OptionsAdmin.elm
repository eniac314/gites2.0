module Bookings.OptionsAdmin exposing (..)

import Auth.AuthPlugin exposing (LogInfo, cmdIfLogged, secureGet, securePost, secureRequest)
import Bookings.BookingsShared exposing (..)
import Dict exposing (..)
import Dict.Extra exposing (find)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import File.Download as Download
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.MarkdownEditor as MarkdownEditor
import Internals.MarkdownParser as MarkdownParser
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (swapAt)
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
import Style.Palette exposing (..)


type alias Model msg =
    { selected : Maybe Int
    , options : Dict Int BookingOption
    , optionNameFr : Maybe String
    , optionNameEn : Maybe String
    , optionPrice : Maybe Float
    , twoNightsPrice : Maybe Float
    , threeNightsPrice : Maybe Float
    , moreThan3NightsPrice : Maybe Float
    , touristTax : Maybe Float
    , article : Maybe MultLangStr
    , markdownEditor : MarkdownEditor.Model msg
    , displayMode : DisplayMode
    , needToSave : Bool
    , outMsg : Msg -> msg
    }


currentBookingOptions : Model msg -> BookingOptions
currentBookingOptions model =
    { twoNightsPrice = model.twoNightsPrice
    , threeNightsPrice = model.threeNightsPrice
    , moreThan3NightsPrice = model.moreThan3NightsPrice
    , touristTax = model.touristTax
    , options =
        Dict.foldr
            (\k v acc -> Dict.insert v.key v acc)
            Dict.empty
            model.options
    }


type DisplayMode
    = Preview
    | EditMarkdown


type Msg
    = NewOption
    | DeleteOption Int
    | EditArticle
    | SwapUp Int
    | SwapDown Int
    | NameInputFr String
    | NameInputEn String
    | SetPrice String
    | SetOneDayPrice String
    | SetDiscountPrice String
    | SetOneWeekPrice String
    | SetTouristTax String
    | SelectOption Int
    | SaveChanges
    | SaveOptionsChanges
    | Saved (Result Http.Error ())
    | Cancel
    | MarkdownEditorMsg MarkdownEditor.Msg
    | GotRateArticle (Result Http.Error MultLangStr)
    | GotBookingOptions (Result Http.Error BookingOptions)
    | DownloadDoc String
    | NoOp


init : (Msg -> msg) -> ( Model msg, Cmd msg )
init outMsg =
    ( { selected = Nothing
      , options = Dict.empty
      , optionNameFr = Nothing
      , optionNameEn = Nothing
      , optionPrice = Nothing
      , twoNightsPrice = Nothing
      , threeNightsPrice = Nothing
      , moreThan3NightsPrice = Nothing
      , touristTax = Nothing
      , article = Nothing
      , markdownEditor =
            MarkdownEditor.init
                French
                (outMsg << MarkdownEditorMsg)
      , displayMode = Preview
      , needToSave = False
      , outMsg = outMsg
      }
    , Cmd.map outMsg <|
        Cmd.batch
            [ getBookingOptions GotBookingOptions
            , Http.get
                { url = "/api/pagesdata/rateArticle"
                , expect =
                    Http.expectJson
                        GotRateArticle
                        (D.field "data" <|
                            D.field "content"
                                decodeMls
                        )
                }
            ]
    )


update : { a | logInfo : LogInfo } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        NewOption ->
            case ( model.optionNameEn, model.optionNameFr, model.optionPrice ) of
                ( Just en, Just fr, Just p ) ->
                    let
                        newOption =
                            { name = MultLangStr en fr
                            , price = p
                            , key = en
                            , picked = False
                            }

                        newModel =
                            { model
                                | options =
                                    Dict.insert
                                        (nextId model.options)
                                        newOption
                                        model.options
                                , optionNameFr = Nothing
                                , optionNameEn = Nothing
                                , optionPrice = Nothing
                            }
                    in
                    ( newModel
                    , saveOptions config.logInfo newModel
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteOption n ->
            let
                newModel =
                    { model | options = Dict.remove n model.options }
            in
            ( newModel, saveOptions config.logInfo newModel )

        EditArticle ->
            ( { model
                | markdownEditor =
                    MarkdownEditor.load
                        model.markdownEditor
                        model.article
                , displayMode = EditMarkdown
              }
            , Cmd.none
            )

        SwapUp id ->
            let
                newOptions =
                    Dict.values model.options
                        |> swapAt id (id - 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList

                newModel =
                    { model | options = newOptions }
            in
            ( newModel
            , saveOptions config.logInfo newModel
            )

        SwapDown id ->
            let
                newOptions =
                    Dict.values model.options
                        |> swapAt id (id + 1)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList

                newModel =
                    { model | options = newOptions }
            in
            ( newModel
            , saveOptions config.logInfo newModel
            )

        NameInputFr name ->
            ( { model
                | optionNameFr =
                    if name == "" then
                        Nothing
                    else
                        Just name
              }
            , Cmd.none
            )

        NameInputEn name ->
            ( { model
                | optionNameEn =
                    if name == "" then
                        Nothing
                    else
                        Just name
              }
            , Cmd.none
            )

        SetPrice price ->
            ( { model
                | optionPrice = String.toFloat price
                , needToSave = True
              }
            , Cmd.none
            )

        SetOneDayPrice price ->
            ( { model
                | twoNightsPrice = String.toFloat price
                , needToSave = True
              }
            , Cmd.none
            )

        SetDiscountPrice price ->
            ( { model
                | threeNightsPrice = String.toFloat price
                , needToSave = True
              }
            , Cmd.none
            )

        SetOneWeekPrice price ->
            ( { model
                | moreThan3NightsPrice = String.toFloat price
                , needToSave = True
              }
            , Cmd.none
            )

        SetTouristTax price ->
            ( { model
                | touristTax = String.toFloat price
                , needToSave = True
              }
            , Cmd.none
            )

        SelectOption n ->
            case Dict.get n model.options of
                Nothing ->
                    ( model, Cmd.none )

                Just { name, price } ->
                    ( { model
                        | selected = Just n
                        , optionNameFr = Just name.fr
                        , optionNameEn = Just name.en
                        , optionPrice = Just price
                      }
                    , Cmd.none
                    )

        SaveChanges ->
            ( model, saveOptions config.logInfo model )

        SaveOptionsChanges ->
            case ( model.selected, ( model.optionNameFr, model.optionNameEn ), model.optionPrice ) of
                ( Just n, ( Just fr, Just en ), Just p ) ->
                    let
                        newModel =
                            { model
                                | options =
                                    Dict.update n
                                        (\mbO ->
                                            case mbO of
                                                Nothing ->
                                                    Nothing

                                                Just o ->
                                                    Just
                                                        { o
                                                            | name =
                                                                MultLangStr en fr
                                                            , price = p
                                                        }
                                        )
                                        model.options
                                , selected = Nothing
                                , optionNameFr = Nothing
                                , optionNameEn = Nothing
                                , optionPrice = Nothing
                            }
                    in
                    ( newModel, saveOptions config.logInfo newModel )

                _ ->
                    ( model, Cmd.none )

        Saved res ->
            case res of
                Ok _ ->
                    ( { model | needToSave = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model
                | selected = Nothing
                , optionNameFr = Nothing
                , optionNameEn = Nothing
                , optionPrice = Nothing
              }
            , Cmd.none
            )

        MarkdownEditorMsg markdownEditorMsg ->
            let
                ( newEditor, mbPluginRes ) =
                    MarkdownEditor.update markdownEditorMsg model.markdownEditor
            in
            case mbPluginRes of
                Nothing ->
                    ( { model | markdownEditor = newEditor }
                    , Cmd.none
                    )

                Just PluginQuit ->
                    ( { model
                        | markdownEditor = newEditor
                        , displayMode = Preview
                      }
                    , Cmd.none
                    )

                Just (PluginData data) ->
                    let
                        newModel =
                            { model
                                | markdownEditor = newEditor
                                , article =
                                    Just data
                                , displayMode = Preview
                            }
                    in
                    ( newModel
                    , saveRateArticle config.logInfo newModel
                    )

        GotRateArticle res ->
            case res of
                Ok a ->
                    ( { model | article = Just a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotBookingOptions res ->
            case res of
                Ok o ->
                    ( { model
                        | twoNightsPrice = o.twoNightsPrice
                        , threeNightsPrice = o.threeNightsPrice
                        , moreThan3NightsPrice = o.moreThan3NightsPrice
                        , touristTax = o.touristTax
                        , options =
                            Dict.values o.options
                                |> List.indexedMap Tuple.pair
                                |> Dict.fromList
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DownloadDoc url ->
            ( model, Download.url url )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


view config model =
    case model.displayMode of
        EditMarkdown ->
            column
                [ width fill
                , height fill
                , padding 45
                , spacing 30
                , Background.color lightGrey
                ]
                [ MarkdownEditor.view config model.markdownEditor ]

        Preview ->
            Element.map model.outMsg <|
                column
                    [ width fill
                    , height fill
                    , padding 45
                    , spacing 30
                    , Background.color lightGrey
                    ]
                    [ column
                        [ centerX
                        , spacing 15
                        , Background.color white
                        , padding 15
                        , Border.rounded 5
                        , Border.color grey
                        , Border.width 1
                        , width (px 1000)
                        ]
                        [ column
                            [ centerX
                            , spacing 15
                            ]
                            [ el
                                [ Font.bold
                                , centerX
                                ]
                                (textM config.lang
                                    (MultLangStr "Base price settings"
                                        "Paramètres bases tarifaires"
                                    )
                                )
                            , Input.text
                                (textInputStyle ++ [ width (px 100) ])
                                { onChange = SetOneDayPrice
                                , text =
                                    Maybe.map String.fromFloat model.twoNightsPrice
                                        |> Maybe.withDefault ""
                                , placeholder =
                                    Nothing
                                , label =
                                    Input.labelLeft
                                        [ centerY
                                        , width (px 150)
                                        ]
                                        (textM config.lang
                                            (MultLangStr
                                                "Price for two nights:"
                                                "Tarif deux nuits:"
                                            )
                                        )
                                }
                            , Input.text
                                (textInputStyle ++ [ width (px 100) ])
                                { onChange = SetDiscountPrice
                                , text =
                                    Maybe.map String.fromFloat model.threeNightsPrice
                                        |> Maybe.withDefault ""
                                , placeholder =
                                    Nothing
                                , label =
                                    Input.labelLeft
                                        [ centerY
                                        , width (px 150)
                                        ]
                                        (textM config.lang
                                            (MultLangStr
                                                "Price for three nights:"
                                                "Tarif trois nuits:"
                                            )
                                        )
                                }
                            , Input.text
                                (textInputStyle ++ [ width (px 100) ])
                                { onChange = SetOneWeekPrice
                                , text =
                                    Maybe.map String.fromFloat model.moreThan3NightsPrice
                                        |> Maybe.withDefault ""
                                , placeholder =
                                    Nothing
                                , label =
                                    Input.labelLeft
                                        [ centerY
                                        , width (px 150)
                                        ]
                                        (textM config.lang
                                            (MultLangStr
                                                "Price for n > 3 nights:"
                                                "Base tarif n > 3 nuits:"
                                            )
                                        )
                                }

                            --, Input.text
                            --    (textInputStyle ++ [ width (px 100) ])
                            --    { onChange = SetTouristTax
                            --    , text =
                            --        Maybe.map String.fromFloat model.touristTax
                            --            |> Maybe.withDefault ""
                            --    , placeholder =
                            --        Nothing
                            --    , label =
                            --        Input.labelLeft
                            --            [ centerY
                            --            , width (px 150)
                            --            ]
                            --            (textM config.lang
                            --                (MultLangStr
                            --                    "Tourist tax:"
                            --                    "Taxe de séjour:"
                            --                )
                            --            )
                            --    }
                            , Input.button
                                (buttonStyle model.needToSave)
                                { onPress =
                                    if model.needToSave then
                                        Just SaveChanges
                                    else
                                        Nothing
                                , label =
                                    textM config.lang <|
                                        MultLangStr "Confirm" "Valider"
                                }
                            ]
                        ]
                    , column
                        [ centerX
                        , spacing 15
                        , Background.color white
                        , padding 15
                        , Border.rounded 5
                        , Border.color grey
                        , Border.width 1
                        , width (px 1000)
                        ]
                        [ column
                            [ centerX
                            , spacing 15
                            ]
                            [ el
                                [ Font.bold
                                , centerX
                                ]
                                (textM config.lang
                                    (MultLangStr "Options control panel"
                                        "Administrateur options"
                                    )
                                )
                            , row
                                [ centerX
                                , spacing 15
                                ]
                                [ Input.text
                                    textInputStyle
                                    { onChange = NameInputEn
                                    , text =
                                        model.optionNameEn
                                            |> Maybe.withDefault ""
                                    , placeholder =
                                        Just <|
                                            Input.placeholder
                                                []
                                                (textM config.lang
                                                    (MultLangStr
                                                        "New option name (english)"
                                                        "Nom nouvelle option (anglais)"
                                                    )
                                                )
                                    , label = Input.labelHidden ""
                                    }
                                , Input.text
                                    textInputStyle
                                    { onChange = NameInputFr
                                    , text =
                                        model.optionNameFr
                                            |> Maybe.withDefault ""
                                    , placeholder =
                                        Just <|
                                            Input.placeholder
                                                []
                                                (textM config.lang
                                                    (MultLangStr
                                                        "Option name (french)"
                                                        "Nom option (français)"
                                                    )
                                                )
                                    , label = Input.labelHidden ""
                                    }
                                , Input.text
                                    (textInputStyle ++ [ width (px 100) ])
                                    { onChange = SetPrice
                                    , text =
                                        Maybe.map String.fromFloat model.optionPrice
                                            |> Maybe.withDefault ""
                                    , placeholder =
                                        Just <|
                                            Input.placeholder
                                                []
                                                (textM config.lang
                                                    (MultLangStr
                                                        "Option price"
                                                        "Prix option"
                                                    )
                                                )
                                    , label = Input.labelHidden ""
                                    }
                                , Input.button
                                    (buttonStyle (hasValidOption model))
                                    { onPress =
                                        if hasValidOption model then
                                            case model.selected of
                                                Just _ ->
                                                    Just SaveOptionsChanges

                                                Nothing ->
                                                    Just NewOption
                                        else
                                            Nothing
                                    , label =
                                        textM config.lang <|
                                            case model.selected of
                                                Just _ ->
                                                    MultLangStr "Confirm" "Valider"

                                                Nothing ->
                                                    MultLangStr "New option" "Nouvelle option"
                                    }
                                ]
                            , optionsView config model
                            ]
                        ]
                    , column
                        [ centerX
                        , spacing 15
                        , Background.color white
                        , padding 15
                        , Border.rounded 5
                        , Border.color grey
                        , Border.width 1
                        , width (px 1000)
                        ]
                        [ Input.button
                            (buttonStyle True ++ [ centerX ])
                            { onPress =
                                Just EditArticle
                            , label =
                                textM config.lang <|
                                    case model.article of
                                        Just a ->
                                            MultLangStr "Edit article" "Modifier Article"

                                        _ ->
                                            MultLangStr "New article" "Nouvel Article"
                            }
                        , case model.article of
                            Just a ->
                                MarkdownParser.renderMarkdown
                                    (strM config.lang a)
                                    DownloadDoc

                            Nothing ->
                                Element.none
                        ]
                    ]


hasValidOption model =
    case ( model.optionNameFr, model.optionNameEn, model.optionPrice ) of
        ( Just _, Just _, Just _ ) ->
            True

        _ ->
            False


optionsView config model =
    let
        optionView ( n, o ) =
            let
                defAttr =
                    [ Events.onClick (SelectOption n) ]
            in
            row
                ([ spacing 15
                 , pointer
                 ]
                    ++ (case model.selected of
                            Just s ->
                                if s == n then
                                    [ Events.onClick Cancel ]
                                else
                                    defAttr

                            Nothing ->
                                defAttr
                       )
                )
                [ el [ width (px 200) ]
                    (textM config.lang o.name)
                , el [ width (px 200) ]
                    (text <| String.fromFloat o.price ++ "€")
                , row [ spacing 7 ]
                    [ Input.button
                        iconsStyle
                        { onPress = Just <| SwapUp n
                        , label =
                            Icons.arrowUp
                                (Icons.defOptions
                                    |> Icons.color black
                                    |> Icons.size 20
                                )
                        }
                    , Input.button
                        iconsStyle
                        { onPress = Just <| SwapDown n
                        , label =
                            Icons.arrowDown
                                (Icons.defOptions
                                    |> Icons.color black
                                    |> Icons.size 20
                                )
                        }
                    , Input.button
                        iconsStyle
                        { onPress = Just <| DeleteOption n
                        , label =
                            Icons.x
                                (Icons.defOptions
                                    |> Icons.color black
                                    |> Icons.size 20
                                )
                        }
                    ]
                ]
    in
    column
        [ centerX
        , spacing 15
        ]
        (Dict.toList model.options
            |> List.map optionView
        )



-------------------------------------------------------------------------------
----------
-- Http --
----------


saveOptions : LogInfo -> Model msg -> Cmd msg
saveOptions logInfo model =
    let
        body =
            E.object
                [ ( "name", E.string "bookingOptions" )
                , ( "content"
                  , encodeBookingOptions
                        { twoNightsPrice = model.twoNightsPrice
                        , threeNightsPrice = model.threeNightsPrice
                        , moreThan3NightsPrice = model.moreThan3NightsPrice
                        , touristTax = model.touristTax
                        , options =
                            Dict.toList model.options
                                |> List.map (\( n, v ) -> ( v.key, v ))
                                |> Dict.fromList
                        }
                  )
                ]
                |> Http.jsonBody
    in
    securePost logInfo
        { url = "api/restricted/pagesdata"
        , body = body
        , expect =
            Http.expectWhatever (model.outMsg << Saved)
        }


saveRateArticle : LogInfo -> Model msg -> Cmd msg
saveRateArticle logInfo model =
    let
        body =
            E.object
                [ ( "name", E.string "rateArticle" )
                , ( "content"
                  , Maybe.map encodeMls model.article
                        |> Maybe.withDefault E.null
                  )
                ]
                |> Http.jsonBody
    in
    securePost logInfo
        { url = "api/restricted/pagesdata"
        , body = body
        , expect =
            Http.expectWhatever (model.outMsg << (\_ -> NoOp))
        }
