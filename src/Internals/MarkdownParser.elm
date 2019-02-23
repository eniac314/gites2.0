module Internals.MarkdownParser exposing (..)

import Browser exposing (element)
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
import Markdown exposing (..)
import Markdown.Block as Block exposing (..)
import Markdown.Inline as Inline exposing (..)
import Style.Palette exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = StrInput String
    | Parse
    | NoOp


type alias Model b i =
    { input : Maybe String
    , blocks : List (Block b i)
    }


init : () -> ( Model b i, Cmd Msg )
init _ =
    ( { input = Nothing
      , blocks = []
      }
    , Cmd.none
    )


update msg model =
    case msg of
        StrInput s ->
            ( { model
                | input =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        Parse ->
            ( { model
                | blocks =
                    case model.input of
                        Just s ->
                            Block.parse Nothing s

                        Nothing ->
                            []
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view model =
    layout [] <|
        column
            []
            [ row
                [ spacing 20 ]
                [ column
                    [ spacing 15
                    , alignTop
                    ]
                    [ Input.multiline
                        [ width (px 500)
                        , height (px 600)
                        ]
                        { onChange = StrInput
                        , text =
                            model.input
                                |> Maybe.withDefault ""
                        , placeholder = Nothing
                        , label = Input.labelHidden ""
                        , spellcheck = False
                        }
                    , Input.button
                        []
                        { onPress =
                            case model.input of
                                Just _ ->
                                    Just Parse

                                _ ->
                                    Nothing
                        , label =
                            el
                                [ Background.color (rgb255 127 127 127)
                                , paddingXY 10 7
                                ]
                                (text "Push me!")
                        }
                    ]
                , column
                    [ alignTop
                    , spacing 15
                    , width fill
                    ]
                    (List.map
                        (\b ->
                            paragraph
                                [ width fill ]
                                [ text <| Debug.toString b ]
                        )
                        model.blocks
                    )
                ]
            , row
                [ spacing 20
                , width fill
                ]
                [ el
                    [ alignTop
                    , width (maximum 550 fill)
                    , Background.color (rgba255 255 0 0 0.3)
                    , clipX
                    , scrollbarX
                    ]
                    (html <|
                        Html.div []
                            (Markdown.toHtml Nothing
                                (Maybe.withDefault "" model.input)
                            )
                    )
                , column
                    [ alignTop
                    , width (maximum 550 fill)
                    , Background.color (rgba255 255 255 0 0.3)
                    , spacing 15
                    ]
                    (blocksToElements model.blocks)
                ]
            ]


subscriptions model =
    Sub.batch []


blocksToElements : List (Block b i) -> List (Element Msg)
blocksToElements blocks =
    List.map (blockToElement 0) blocks


blockToElement : Int -> Block b i -> Element Msg
blockToElement offset block =
    case block of
        BlankLine s ->
            el [] (html <| Html.br [] [])

        ThematicBreak ->
            Element.none

        Heading raw level inlines ->
            headings raw level inlines

        CodeBlock _ raw ->
            el []
                (html <|
                    Html.pre []
                        [ Html.text raw ]
                )

        Paragraph raw inlines ->
            wrappedRow
                []
                (List.concatMap inlinesToElements inlines)

        BlockQuote blocks ->
            column
                [ spacing 15 ]
                (List.map (blockToElement offset) blocks)

        List listblock llistBlocks ->
            let
                bullet off =
                    if off == 0 then
                        "●"
                    else if off == 1 then
                        "-"
                    else
                        "->"

                liView bs =
                    case bs of
                        [] ->
                            []

                        (List _ _) :: _ ->
                            List.map (blockToElement (offset + 1)) bs

                        _ ->
                            [ row
                                [ width fill
                                , spacing 10
                                ]
                                [ el [ alignTop ] (text <| bullet offset)
                                , paragraph [] (List.map (blockToElement (offset + 1)) bs)
                                ]
                            ]
            in
            column
                [ paddingXY 40 0
                ]
                (List.concatMap liView llistBlocks)

        PlainInlines inlines ->
            paragraph
                []
                (List.concatMap inlinesToElements inlines)

        Block.Custom b llistBlocks ->
            Element.none


headings raw level inlines =
    paragraph
        [ Region.heading level
        , if level == 1 then
            Font.color blue
          else
            Font.color purple
        , Font.size (16 * (6 - level))
        ]
        (List.concatMap inlinesToElements inlines)


inlinesToElements : Inline i -> List (Element Msg)
inlinesToElements inline =
    case inline of
        Text s ->
            [ paragraph [] [ text s ] ]

        HardLineBreak ->
            [ el [] (html <| Html.br [] []) ]

        CodeInline s ->
            [ text s ]

        Link url mbTitle inlines ->
            [ link
                [ Font.underline
                , Font.color (rgb255 0 0 255)
                ]
                { url = url
                , label = text <| Inline.extractText inlines
                }
            ]

        Image url mbTitle inlines ->
            [ --el
              --    [ Background.color (rgb255 0 0 255)
              --    , Background.image url
              --    , width (px 400)
              --    --, height (px 233)
              --    ]
              --    (text "image")
              image
                [ width fill ]
                { src = url
                , description = Inline.extractText inlines
                }
            ]

        HtmlInline s _ _ ->
            [ text s ]

        Emphasis n inlines ->
            [ paragraph
                (if n == 1 then
                    [ Font.italic ]
                 else if n == 2 then
                    [ Font.bold ]
                 else if n == 3 then
                    [ Font.italic
                    , Font.bold
                    ]
                 else
                    []
                )
                (List.concatMap inlinesToElements inlines)
            ]

        Inline.Custom i inlines ->
            []


noAttr =
    htmlAttribute <| HtmlAttr.class ""
