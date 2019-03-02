module Internals.MarkdownParser exposing (renderMarkdown)

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
import Style.Helpers exposing (..)
import Style.Palette exposing (..)


renderMarkdown : String -> Element msg
renderMarkdown s =
    Block.parse Nothing s
        |> blocksToElements
        |> column
            [ width fill
            , spacing 15
            ]


blocksToElements : List (Block b i) -> List (Element msg)
blocksToElements blocks =
    List.map (blockToElement 0) blocks


blockToElement : Int -> Block b i -> Element msg
blockToElement offset block =
    case block of
        BlankLine s ->
            Element.none

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
            paragraph
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


inlinesToElements : Inline i -> List (Element msg)
inlinesToElements inline =
    case inline of
        Text s ->
            [ text s ]

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
            [ image
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
