module GenericPage.GenericPage exposing (..)

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
import Gallery.Carousel as Carousel
import GenericPage.GenericPageShared exposing (..)
import Http exposing (..)
import MultLang.MultLang exposing (..)


type alias Model msg =
    { workingDirectory : String
    , content : GenericPageContent
    , outMsg : Msg -> msg
    , carousels : Dict String (Carousel.Model msg)
    }


type Msg
    = GotGenericPageContent (Result Http.Error GenericPageContent)
    | CarouselMsg String Carousel.Msg
    | NoOp


init : (Msg -> msg) -> String -> ( Model msg, Cmd msg )
init outMsg wd =
    ( { workingDirectory = wd
      , outMsg = outMsg
      , content = []
      , carousels = Dict.empty
      }
    , Cmd.batch
        [ getGenericPageContent (outMsg << GotGenericPageContent) wd
        ]
    )


subscriptions model =
    Sub.batch
        (Dict.map (\id c -> Carousel.subscriptions c) model.carousels
            |> Dict.values
        )


update : { a | width : Int } -> Msg -> Model msg -> Model msg
update config msg model =
    case msg of
        GotGenericPageContent res ->
            case res of
                Ok content ->
                    { model
                        | content = content
                        , carousels =
                            initCarousels content
                                (\id -> model.outMsg << CarouselMsg id)
                    }

                Err e ->
                    model

        CarouselMsg id carMsg ->
            case Dict.get id model.carousels of
                Just carousel ->
                    { model
                        | carousels =
                            Dict.insert id
                                (Carousel.update
                                    { maxWidth = config.width }
                                    carMsg
                                    carousel
                                )
                                model.carousels
                    }

                Nothing ->
                    model

        NoOp ->
            model


view : { a | lang : Lang, width : Int } -> Model msg -> Element msg
view config model =
    column
        [ width (maximum 1000 fill)
        , height (minimum 500 fill)
        , centerX
        , padding 15
        , spacing 15
        ]
        (List.map
            (genericPageItemView
                { lang = config.lang
                , width = config.width
                , carousels = model.carousels
                }
            )
            model.content
        )
