module Gallery.GalleryPage exposing (..)

import Browser.Navigation exposing (pushUrl)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Element.Region as Region
import Gallery.Gallery as Gallery exposing (..)
import Gallery.GalleryShared exposing (..)
import Http exposing (..)
import Internals.Helpers exposing (..)
import Internals.MarkdownParser as MarkdownParser
import MultLang.MultLang exposing (..)
import Style.Helpers exposing (..)
import Style.Palette exposing (..)
import Url


type alias Model msg =
    { galleries : Dict String (Gallery.Model msg)
    , mainArticle : Maybe MultLangStr
    , outMsg : Msg -> msg
    }


type Msg
    = GalleryMsg String Gallery.Msg
    | OpenGallery String
    | GotGalleryMetas (Result Http.Error DetailsPage)
    | NoOp


type alias Config a =
    { a
        | width : Int
        , lang : Lang
        , key : Browser.Navigation.Key
    }


init outMsg =
    ( { galleries = Dict.empty
      , mainArticle = Nothing
      , outMsg = outMsg
      }
    , Cmd.batch
        [ getDetailsPage (outMsg << GotGalleryMetas)
        ]
    )


subscriptions model =
    Sub.batch
        (Dict.map (\t g -> Gallery.subscriptions g) model.galleries
            |> Dict.values
        )


update : Config a -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        GalleryMsg title msg_ ->
            case
                Dict.get title model.galleries
                    |> Maybe.map
                        (Gallery.update config msg_)
            of
                Just ( gallery, cmd ) ->
                    ( { model
                        | galleries =
                            Dict.insert title gallery model.galleries
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        OpenGallery title ->
            ( model, pushUrl config.key <| "details/" ++ title )

        GotGalleryMetas res ->
            case res of
                Ok { mainArticle, galleries } ->
                    ( { model
                        | galleries =
                            Dict.map
                                (\k gm ->
                                    Gallery.init gm
                                        (model.outMsg << GalleryMsg k)
                                )
                                galleries
                        , mainArticle = mainArticle
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type alias ViewConfig =
    { lang : Lang
    , url : Url.Url
    , width : Int
    }


view : ViewConfig -> Model msg -> Element msg
view config model =
    column
        [ spacing 15
        , padding 15
        , width (maximum 1000 fill)
        , centerX
        , Font.size 16
        ]
        (case
            String.split "/"
                (Url.percentDecode config.url.path
                    |> Maybe.withDefault ""
                )
         of
            "" :: "details" :: [] ->
                [ homeView config model ]

            "" :: "details" :: galleryTitle :: [] ->
                [ galleryView config model galleryTitle ]

            _ ->
                []
        )


homeView : ViewConfig -> Model msg -> Element msg
homeView config model =
    column
        [ centerX
        , spacing 15
        ]
        [ case model.mainArticle of
            Just a ->
                MarkdownParser.renderMarkdown
                    (strM config.lang a)

            Nothing ->
                Element.none
        , column
            [ spacing 10 ]
            (chunkedRows
                (min config.width 1000)
                (bestFit 200)
                (Dict.map
                    (\k v ->
                        let
                            defTitleImg =
                                case v.titleImg of
                                    Just url ->
                                        url

                                    Nothing ->
                                        List.head v.imagesSrcs
                                            |> Maybe.withDefault ""
                        in
                        { v | titleImg = Just defTitleImg }
                    )
                    model.galleries
                    |> Dict.values
                    |> List.sortBy .ordering
                    |> List.map (imgBlockView config.lang (model.outMsg << OpenGallery))
                )
            )
        ]


galleryView : ViewConfig -> Model msg -> String -> Element msg
galleryView config model title =
    case Dict.get title model.galleries of
        Just gallery ->
            column
                [ width (maximum 1000 fill)
                , height (minimum 500 fill)
                , centerX
                , padding 15
                , spacing 15
                ]
                [ case gallery.header of
                    Just h ->
                        MarkdownParser.renderMarkdown
                            (strM config.lang h)

                    Nothing ->
                        Element.none
                , Gallery.view config gallery
                , case gallery.article of
                    Just a ->
                        MarkdownParser.renderMarkdown
                            (strM config.lang a)

                    Nothing ->
                        Element.none
                , link
                    [ centerX
                    , padding 10
                    , Background.color white
                    , Font.family
                        [ Font.typeface "Montserrat"
                        , Font.sansSerif
                        ]
                    , Border.width 1
                    , Border.rounded 2
                    , focused [ Border.glow (rgb 1 1 1) 0 ]
                    ]
                    { url = "/details"
                    , label = textM config.lang (MultLangStr "Go back" "Retour")
                    }
                ]

        Nothing ->
            Element.none
