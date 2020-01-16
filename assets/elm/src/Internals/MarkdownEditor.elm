module Internals.MarkdownEditor exposing (..)

import Dict exposing (..)
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
import Html as Html
import Html.Attributes as HtmlAttr
import Internals.DocController as DocController
import Internals.Helpers exposing (..)
import Internals.MarkdownParser as MarkdownParser exposing (..)
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)
import Style.Palette exposing (..)


type alias Model msg =
    { inputs : Maybe MultLangStr
    , previewLang : Lang
    , pickedLink : Maybe String
    , linkPickerOpen : Bool
    , outMsg : Msg -> msg
    }


init defaultLang outMsg =
    { inputs = Nothing
    , previewLang = defaultLang
    , pickedLink = Nothing
    , linkPickerOpen = False
    , outMsg = outMsg
    }


load model mbInputs =
    { model | inputs = mbInputs }


type Msg
    = StrInputFr String
    | StrInputEn String
    | TooglePreviewLang
    | GoBack
    | Save
    | ToogleLinkPickerOpen
    | PickLink String
    | DownloadDoc String
    | NoOp


update : Msg -> Model msg -> ( Model msg, Maybe (PluginResult MultLangStr) )
update msg model =
    let
        baseMls =
            model.inputs
                |> Maybe.withDefault (MultLangStr "" "")
    in
    case msg of
        StrInputFr s ->
            ( { model | inputs = Just { baseMls | fr = s } }
            , Nothing
            )

        StrInputEn s ->
            ( { model | inputs = Just { baseMls | en = s } }
            , Nothing
            )

        TooglePreviewLang ->
            ( { model
                | previewLang =
                    if model.previewLang == English then
                        French
                    else
                        English
              }
            , Nothing
            )

        GoBack ->
            ( { model | inputs = Nothing }
            , Just PluginQuit
            )

        Save ->
            case model.inputs of
                Nothing ->
                    ( model, Nothing )

                Just data ->
                    if data.fr == "" && data.en == "" then
                        ( model, Nothing )
                    else
                        ( { model | inputs = Nothing }
                        , Just <| PluginData data
                        )

        ToogleLinkPickerOpen ->
            ( { model | linkPickerOpen = not model.linkPickerOpen }
            , Nothing
            )

        PickLink url ->
            ( { model | pickedLink = Just url }
            , Nothing
            )

        DownloadDoc url ->
            ( model, Nothing )

        NoOp ->
            ( model, Nothing )


type alias ViewConfig a =
    { a
        | lang : Lang
        , width : Int
        , documents : Dict String String
    }


view : ViewConfig a -> Model msg -> Element msg
view config model =
    let
        baseMls =
            model.inputs
                |> Maybe.withDefault (MultLangStr "" "")
    in
    Element.map model.outMsg <|
        column
            [ width fill
            , height fill
            , Background.color lightGrey
            ]
            [ column
                [ paddingXY 10 10
                , spacing 15
                , centerX
                , width fill
                ]
                [ (if config.width < 970 then
                    column
                   else
                    row
                  )
                    [ spacing 15
                    , width fill
                    , centerX
                    ]
                    [ Input.multiline
                        [ width fill
                        , height (px 350)
                        , centerX
                        , focused [ Border.glow (rgb 1 1 1) 0 ]
                        ]
                        { onChange = StrInputFr
                        , text =
                            baseMls.fr
                        , placeholder = Nothing
                        , label =
                            Input.labelAbove
                                [ padding 10 ]
                                (textM config.lang (MultLangStr "French" "Français"))
                        , spellcheck = False
                        }
                    , Input.multiline
                        [ width fill
                        , height (px 350)
                        , centerX
                        , focused [ Border.glow (rgb 1 1 1) 0 ]
                        ]
                        { onChange = StrInputEn
                        , text =
                            baseMls.en
                        , placeholder = Nothing
                        , label =
                            Input.labelAbove
                                [ padding 10 ]
                                (textM config.lang (MultLangStr "English" "Anglais"))
                        , spellcheck = False
                        }
                    ]
                , DocController.linkPicker
                    config.documents
                    { isOpen = model.linkPickerOpen
                    , picked = model.pickedLink
                    , toogleOpen = ToogleLinkPickerOpen
                    , handler = PickLink
                    , noOp = NoOp
                    , lang = config.lang
                    }
                , row
                    [ width fill
                    , padding 15
                    , spacing 15
                    , Border.width 1
                    , Border.rounded 2
                    ]
                    [ MarkdownParser.renderMarkdown
                        (strM model.previewLang baseMls)
                        DownloadDoc
                    , column
                        []
                        [ image
                            [ width (px 30)
                            , Events.onClick TooglePreviewLang
                            , pointer
                            ]
                            { src =
                                case model.previewLang of
                                    English ->
                                        "/images/english.png"

                                    French ->
                                        "/images/french.png"
                            , description = ""
                            }
                        ]
                    ]
                , row
                    [ centerX
                    , spacing 10
                    ]
                    [ Input.button
                        (buttonStyle True)
                        { onPress = Just GoBack
                        , label =
                            textM config.lang (MultLangStr "Back" "Retour")
                        }
                    , Input.button
                        (buttonStyle True)
                        { onPress = Just Save
                        , label =
                            textM config.lang (MultLangStr "Save and quit" "Valider")
                        }
                    ]
                ]
            ]
