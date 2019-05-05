module Internals.MarkdownParser exposing (renderMarkdown)

import Browser exposing (element)
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
            , Font.family
                [ Font.typeface "Lato"
                , Font.sansSerif
                ]
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
            el
                [ width fill
                , Border.color grey
                , Border.width 1
                ]
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
                (List.concatMap (inlinesToElements []) inlines)

        BlockQuote blocks ->
            column
                [ spacing 15 ]
                (List.map (blockToElement offset) blocks)

        List listblock llistBlocks ->
            let
                bullet off =
                    if off == 0 then
                        "•"
                    else if off == 1 then
                        "◦"
                    else
                        "‣"

                liView ( i, bs ) =
                    case bs of
                        [] ->
                            []

                        (List _ _) :: _ ->
                            List.map (blockToElement (offset + 1)) bs

                        _ ->
                            [ row
                                [ width fill
                                , spacing 5
                                ]
                                [ case listblock.type_ of
                                    Unordered ->
                                        el [ alignTop ] (text <| bullet offset)

                                    Ordered start ->
                                        el [ alignTop ] (text <| String.fromInt (start + i) ++ ". ")
                                , paragraph [] (List.map (blockToElement (offset + 1)) bs)
                                ]
                            ]
            in
            column
                [ spacing 10
                , paddingXY 0 10
                ]
                (List.concatMap liView (List.indexedMap Tuple.pair llistBlocks))

        PlainInlines inlines ->
            paragraph
                []
                (List.concatMap (inlinesToElements []) inlines)

        Block.Custom b llistBlocks ->
            Element.none


headings raw level inlines =
    let
        headingStyles =
            Dict.fromList
                [ ( 1
                  , [ Font.size 45 ]
                  )
                , ( 2
                  , [ Font.size 35 ]
                  )
                , ( 3
                  , [ Font.size 30 ]
                  )
                , ( 4
                  , [ Font.size 25 ]
                  )
                , ( 5
                  , [ Font.size 15 ]
                  )
                , ( 6
                  , [ Font.size 10 ]
                  )
                ]
    in
    paragraph
        ([ Region.heading level
         , Font.color black
         , Font.family
            [ Font.typeface "Crimson Text"
            , Font.serif
            ]
         ]
            ++ (Dict.get level headingStyles
                    |> Maybe.withDefault []
               )
        )
        (List.concatMap (inlinesToElements []) inlines)


inlinesToElements : List (Attribute msg) -> Inline i -> List (Element msg)
inlinesToElements attrs inline =
    case inline of
        Text s ->
            [ el attrs (text s) ]

        HardLineBreak ->
            [ el attrs (html <| Html.br [] []) ]

        CodeInline s ->
            [ el attrs (text s) ]

        Link url mbTitle inlines ->
            [ link
                (attrs
                    ++ [ Font.underline
                       , Font.color lightBlue
                       ]
                )
                { url = url
                , label = text <| Inline.extractText inlines
                }
            ]

        Image url mbTitle inlines ->
            [ image
                (attrs ++ [ width fill ])
                { src = url
                , description = Inline.extractText inlines
                }
            ]

        HtmlInline s _ _ ->
            [ el attrs (text s) ]

        Emphasis n inlines ->
            let
                attrs_ =
                    attrs
                        ++ (if n == 1 then
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
            in
            List.concatMap (inlinesToElements attrs_) inlines

        Inline.Custom i inlines ->
            []
