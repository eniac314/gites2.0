module FrontPage.MarkdownEditor exposing (..)

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
import Internals.Helpers exposing (..)
import Internals.MarkdownParser as MarkdownParser exposing (..)
import MultLang.MultLang exposing (..)
import Style.Palette exposing (..)
import Style.Helpers exposing (..)
import Style.Icons as Icons exposing (..)


type alias Model msg =
    { inputs : Maybe MultLangStr
    , previewLang : Lang
    , outMsg : Msg -> msg
    }


init defaultLang outMsg =
    { inputs = Nothing
    , previewLang = defaultLang
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

            NoOp ->
                ( model, Nothing )


view : { a | lang : Lang } -> Model msg -> Element msg
view config model =
    let
        baseMls =
            model.inputs
                |> Maybe.withDefault (MultLangStr "" "")
    in
        Element.map model.outMsg <|
            column
                [ width fill
                , Background.color lightGrey
                ]
                [ column
                    [ paddingXY 10 10
                    , spacing 15
                    , centerX
                    , centerY
                    ]
                    [ Input.multiline
                        [ width (px 500)
                        , height (px 600)
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
                        [ width (px 500)
                        , height (px 600)
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
                    , row
                        [ width fill
                        , padding 15
                        , spacing 15
                        , Border.width 1
                        , Border.rounded 2
                        ]
                        [ MarkdownParser.renderMarkdown
                            (strM model.previewLang baseMls)
                        , Input.button
                            []
                            { onPress = Just TooglePreviewLang
                            , label =
                                Icons.eye
                                    (Icons.defOptions
                                        |> Icons.color black
                                        |> Icons.size 25
                                    )
                            }
                        ]
                    , row
                        [ Background.color lightGreen
                        , centerX
                        ]
                        [ Input.button
                            buttonStyle
                            { onPress = Just GoBack
                            , label =
                                textM config.lang (MultLangStr "Back" "Retour")
                            }
                        , Input.button
                            buttonStyle
                            { onPress = Just Save
                            , label =
                                textM config.lang (MultLangStr "Save and quit" "Valider")
                            }
                        ]
                    ]
                ]
