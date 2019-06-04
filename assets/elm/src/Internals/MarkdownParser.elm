module Internals.MarkdownParser exposing (InlineStyle(..), StyleAttribute(..), attribute, blockToElement, blocksToElements, headings, inlineStyle, inlineStyles, inlinesToElements, renderMarkdown, styleAttribute, styleAttributes, value)

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
import Internals.Helpers exposing (awsUrl)
import Markdown exposing (..)
import Markdown.Block as Block exposing (..)
import Markdown.Config exposing (defaultOptions)
import Markdown.Inline as Inline exposing (..)
import Parser exposing (..)
import Style.Helpers exposing (..)
import Style.Palette exposing (..)


renderMarkdown : String -> (String -> msg) -> Element msg
renderMarkdown s downloadHandler =
    Block.parse (Just { defaultOptions | softAsHardLineBreak = True }) s
        |> blocksToElements downloadHandler
        |> column
            [ width fill
            , spacing 15
            , Font.family
                [ Font.typeface "Lato"
                , Font.sansSerif
                ]
            ]


blocksToElements : (String -> msg) -> List (Block b i) -> List (Element msg)
blocksToElements downloadHandler blocks =
    List.map (blockToElement downloadHandler 0) blocks


blockToElement : (String -> msg) -> Int -> Block b i -> Element msg
blockToElement downloadHandler offset block =
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
            headings downloadHandler raw level inlines

        CodeBlock _ raw ->
            el []
                (html <|
                    Html.pre []
                        [ Html.text raw ]
                )

        Paragraph raw inlines ->
            paragraph
                [ Font.family
                    [ Font.typeface "times" ]
                , Font.size 18
                ]
                (List.concatMap (inlinesToElements downloadHandler []) inlines)

        BlockQuote blocks ->
            column
                [ spacing 15 ]
                (List.map (blockToElement downloadHandler offset) blocks)

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
                            List.map (blockToElement downloadHandler (offset + 1)) bs

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
                                , paragraph [] (List.map (blockToElement downloadHandler (offset + 1)) bs)
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
                (List.concatMap (inlinesToElements downloadHandler []) inlines)

        Block.Custom b llistBlocks ->
            Element.none


headings downloadHandler raw level inlines =
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
        (List.concatMap (inlinesToElements downloadHandler []) inlines)



--parseCustomStyles : Inline i -> List (Inline InlineStyle)
--parseCustomStyles inline =
--    case inline of
--        Text s ->
--            case Parser.run inlineStyles s of
--                Ok res ->
--                    List.map Custom res


inlinesToElements : (String -> msg) -> List (Attribute msg) -> Inline i -> List (Element msg)
inlinesToElements downloadHandler attrs inline =
    case inline of
        Text s ->
            [ el attrs (text s) ]

        HardLineBreak ->
            [ el attrs (html <| Html.br [] []) ]

        CodeInline s ->
            [ el attrs (text s) ]

        Link url mbTitle inlines ->
            if String.contains "Documents/" url then
                [ el
                    [ mouseOver
                        [ Font.color blue
                        ]
                    , Font.underline
                    , Font.color lightBlue
                    , pointer
                    , Events.onClick (downloadHandler url)
                    ]
                    (text <| Inline.extractText inlines)
                ]

            else
                [ (if String.startsWith "/" url then
                    link

                   else
                    newTabLink
                  )
                    (attrs
                        ++ [ mouseOver
                                [ Font.color blue
                                ]
                           , Font.underline
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
            List.concatMap (inlinesToElements downloadHandler attrs_) inlines

        Inline.Custom i inlines ->
            []



-------------------------------------------------------------------------------
-------------------------
-- Inline style parser --
-------------------------


type InlineStyle
    = Styled
        { styled : String
        , attrs : List StyleAttribute
        }
    | Regular String


type StyleAttribute
    = Font String
    | FontSize Int
    | Color String


inlineStyles : Parser (List InlineStyle)
inlineStyles =
    let
        helper styles =
            oneOf
                [ succeed (\style -> Loop (style :: styles))
                    |= inlineStyle
                    |> backtrackable
                , symbol "["
                    |> Parser.map (\_ -> Loop (Regular "[" :: styles))
                , succeed (\reg -> Loop (Regular reg :: styles))
                    |= (chompUntil "["
                            |> getChompedString
                       )
                    |> backtrackable
                , succeed
                    (\reg ->
                        Done
                            (List.reverse
                                (Regular reg :: styles)
                                |> concatRegs "" []
                            )
                    )
                    |= (chompUntilEndOr "\n"
                            |> getChompedString
                       )
                ]

        concatRegs acc res xs =
            case xs of
                [] ->
                    if acc == "" then
                        res

                    else
                        List.reverse <| Regular acc :: res

                (Styled s) :: xs_ ->
                    if acc == "" then
                        concatRegs "" (Styled s :: res) xs_

                    else
                        concatRegs "" (Styled s :: Regular acc :: res) xs_

                (Regular r) :: xs_ ->
                    concatRegs (acc ++ r) res xs_
    in
    loop [] helper


inlineStyle : Parser InlineStyle
inlineStyle =
    succeed (\s attrs -> Styled { styled = s, attrs = attrs })
        |. symbol "["
        |= (chompUntil "]"
                |> getChompedString
           )
        |. symbol "]"
        |. spaces
        |. symbol "{"
        |. spaces
        |. keyword "style"
        |. spaces
        |. symbol "|"
        |. spaces
        |= styleAttributes


styleAttributes : Parser (List StyleAttribute)
styleAttributes =
    let
        helper attrs =
            oneOf
                [ succeed (\attr -> Loop (attr :: attrs))
                    |= styleAttribute
                    |. spaces
                    |. symbol ","
                    |. spaces
                    |> backtrackable
                , succeed (\attr -> Loop (attr :: attrs))
                    |= styleAttribute
                    |. spaces
                    |. symbol "}"
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse attrs))
                ]
    in
    loop [] helper


styleAttribute : Parser StyleAttribute
styleAttribute =
    oneOf
        [ attribute Font "police" value
            |> backtrackable
        , attribute FontSize "taille" int
            |> backtrackable
        , attribute Color "couleur" value
        ]


attribute : (a -> b) -> String -> Parser a -> Parser b
attribute sAttr name valueParser =
    succeed sAttr
        |. keyword name
        |. spaces
        |. symbol ":"
        |. spaces
        |= valueParser


value : Parser String
value =
    chompWhile
        (\c ->
            ((c /= ',')
                || (c /= '}')
            )
                && Char.isAlphaNum c
        )
        |> getChompedString
