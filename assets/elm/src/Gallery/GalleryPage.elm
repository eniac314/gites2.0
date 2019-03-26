module Gallery.GalleryPage exposing (..)

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
import Internals.Helpers exposing (..)
import MultLang.MultLang exposing (..)
import Url


type alias Model msg =
    { galleries : Dict String (Gallery.Model Msg)
    , outMsg : Msg -> msg
    }


type Msg
    = GalleryMsg String Gallery.Msg
    | NoOp


type alias Config a =
    { a
        | width : Int
        , lang : Lang
    }


init outMsg =
    ( { galleries = Dict.empty
      , outMsg = outMsg
      }
    , Cmd.none
    )


subscriptions model =
    Sub.map model.outMsg <|
        Sub.batch
            (Dict.map (\t g -> Gallery.subscriptions g) model.galleries
                |> Dict.values
            )


update : Config a -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg ({ galleries } as model) =
    case msg of
        GalleryMsg title msg_ ->
            case
                Dict.get title galleries
                    |> Maybe.map
                        (Gallery.update config msg_)
            of
                Just ( gallery, cmd ) ->
                    ( { model
                        | galleries =
                            Dict.insert title gallery model.galleries
                      }
                    , Cmd.map model.outMsg cmd
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
    Element.map model.outMsg <|
        column
            [ spacing 15
            , padding 15
            , width (maximum 1000 fill)
            , centerX
            , Font.size 16
            ]
            (case String.split "/" config.url.path of
                "" :: "presentation" :: [] ->
                    [ homeView config model ]

                "" :: "presentation" :: galleryTitle :: [] ->
                    [ galleryView config model galleryTitle ]

                _ ->
                    []
            )


homeView : ViewConfig -> Model msg -> Element Msg
homeView config model =
    let
        imgBlockView ( title, titleImg ) =
            column
                [ spacing 10
                , padding 10
                , width (px 200)
                , alignTop
                , mouseOver
                    [ alpha 0.7 ]
                , Border.rounded 5
                ]
                [ el
                    [ width (px 190)
                    , height (px 190)
                    , centerX
                    , Background.image <|
                        awsUrl
                            ++ title
                            ++ "/thumbs/"
                            ++ titleImg
                    ]
                    Element.none
                ]
    in
    column
        []
        (chunkedRows
            (min config.width 1000)
            (bestFit 200)
            (Dict.map (\k v -> ( k, v.title.en )) model.galleries
                |> Dict.values
                |> List.map imgBlockView
            )
        )


galleryView : ViewConfig -> Model msg -> String -> Element Msg
galleryView config model title =
    case Dict.get title model.galleries of
        Just gallery ->
            Gallery.view config gallery

        Nothing ->
            Element.none
