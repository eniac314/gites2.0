module GenericPage.GenericPage exposing (Model, Msg(..), init, subscriptions, update, view)

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
    | DownloadDoc String
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


update : { a | width : Int } -> Msg -> Model msg -> ( Model msg, Cmd msg )
update config msg model =
    case msg of
        GotGenericPageContent res ->
            case res of
                Ok content ->
                    ( { model
                        | content = content
                        , carousels =
                            initCarousels content
                                (\id -> model.outMsg << CarouselMsg id)
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( model, Cmd.none )

        CarouselMsg id carMsg ->
            case Dict.get id model.carousels of
                Just carousel ->
                    ( { model
                        | carousels =
                            Dict.insert id
                                (Carousel.update
                                    { maxWidth = config.width }
                                    carMsg
                                    carousel
                                )
                                model.carousels
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DownloadDoc url ->
            ( model, Download.url url )

        NoOp ->
            ( model, Cmd.none )


view : { a | lang : Lang, width : Int, showGoogleMaps : Bool } -> Model msg -> Element msg
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
                , downloadHandler = model.outMsg << DownloadDoc
                , showGoogleMaps = config.showGoogleMaps
                }
            )
            model.content
        )
