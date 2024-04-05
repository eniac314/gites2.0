port module PdfGen.Contrat exposing (..)

--import Style.Palette exposing (..)

import Bookings.BookingsShared exposing (nightsCount, roundC_)
import Bookings.DatePicker.Date exposing (formatDate)
import Color exposing (..)
import Date exposing (fromPosix)
import Dict exposing (..)
import Html
import Html.Attributes as HA
import Json.Decode as D
import String.Extra
import Time exposing (millisToPosix)
import Ui exposing (..)
import Ui.Font as Font
import Ui.Prose as Prose


port savePdf : String -> Cmd msg


port savePdfProgress : (D.Value -> msg) -> Sub msg


view config model =
    embed [] <|
        clipped [ width (px 0), height (px 0) ] <|
            column []
                [ html <|
                    Html.node "style"
                        []
                        [ Html.text <|
                            """ li.s.e{
  display:list-item;
       
}
                    
                """
                        ]
                , column
                    [ padding 50
                    , width (px 1240)
                    , height (px 1754)

                    --, border 1
                    , Font.size 20
                    , htmlAttribute <| HA.class "contratPDF"
                    ]
                    [ title
                    , el [ Font.center, Font.underline, paddingWith { top = 60, left = 0, right = 0, bottom = 30 } ] (text "Entre les soussignés :")
                    , bailleur
                    , el [ Font.center, Font.underline, padding 20 ] (text "Et :")
                    , preneur config model
                    , recap config model
                    , montant config model
                    , el [ paddingXY 0 30 ] (text "Ci-joint les conditions générales de location ainsi que le descriptif des lieux loués.")
                    , signaturesDate config
                    ]
                , condGen config
                ]


title =
    column
        [ Font.center
        , Font.weight Font.bold
        , spacing 15
        , Font.size 22
        , padding 10
        , background lightGrey
        ]
        [ text "CONTRAT DE LOCATION DU MEUBLE DE TOURISME LE VIEUX LILAS"
        , el [ Font.size 18, Font.center ] (text "6 personnes maximum")
        ]


bailleur =
    column
        [ spacing 10 ]
        [ column
            [ spacing 20
            , paddingXY 10 15
            , border 1
            ]
            [ row
                [ spacing 200 ]
                [ row
                    [ spacing 5, width shrink ]
                    [ el [ Font.weight Font.bold ] (text "Nom :")
                    , el [] (text "Gillard")
                    ]
                , row
                    [ spacing 5, width shrink ]
                    [ el [ Font.weight Font.bold ] (text "Prénom :")
                    , el [] (text "Sylvie")
                    ]
                ]
            , row
                [ spacing 5, width shrink ]
                [ el [ Font.weight Font.bold ] (text "Adresse :")
                , el [] (text "371, allée de la Plage, 63790 MUROL")
                ]
            , row
                [ spacing 5, width shrink ]
                [ el [ Font.weight Font.bold ] (text "Mail :")
                , el [] (text "sylvie.gillard@laposte.net")
                ]
            ]
        , el [ Font.underline ] (text "dénommé le bailleur d’une part,")
        ]


preneur config model =
    case model.pickedBooking of
        Just { firstName, lastName, address, addAddress, postcode, city, country, email, phone1 } ->
            column
                [ spacing 10 ]
                [ column
                    [ spacing 20
                    , paddingXY 10 15
                    , border 1
                    ]
                    [ row
                        [ spacing 200 ]
                        [ row
                            [ spacing 5, width shrink ]
                            [ el [ Font.weight Font.bold ] (text "Nom :")
                            , el [] (text <| String.Extra.toSentenceCase lastName)
                            ]
                        , row
                            [ spacing 5, width shrink ]
                            [ el [ Font.weight Font.bold ] (text "Prénom :")
                            , el [] (text <| String.Extra.toSentenceCase firstName)
                            ]
                        ]
                    , row
                        [ spacing 5 ]
                        [ el [ Font.weight Font.bold, width shrink ] (text "Adresse :")
                        , Prose.paragraph
                            []
                            [ text <|
                                address
                                    ++ ", "
                                    ++ (case addAddress of
                                            Just ad ->
                                                ad ++ ", "

                                            Nothing ->
                                                ""
                                       )
                                    ++ String.fromInt postcode
                                    ++ " "
                                    ++ city
                                    ++ ", "
                                    ++ country
                            ]
                        ]
                    , row
                        [ spacing 5, width shrink ]
                        [ el [ Font.weight Font.bold ] (text "Tel :")
                        , el [] (text phone1)
                        ]
                    , row
                        [ spacing 5, width shrink ]
                        [ el [ Font.weight Font.bold ] (text "Mail :")
                        , el [] (text email)
                        ]
                    ]
                , el [ Font.underline ] (text "dénommé le preneur d’une part,")
                ]

        Nothing ->
            none


recap config model =
    case model.pickedBooking of
        Just { checkIn, checkOut, nbrKids, nbrAdults, pets } ->
            let
                nc =
                    nightsCount checkIn checkOut
            in
            column
                [ paddingXY 0 30
                , spacing 20
                ]
                [ row
                    [ width shrink, spacing 5 ]
                    [ text "Il a été convenu d'une location pour la période du "
                    , el [ Font.weight Font.bold ] (text <| formatDate config.lang checkIn)
                    , text "au"
                    , el [ Font.weight Font.bold ] (text <| formatDate config.lang checkOut)
                    , el
                        []
                        (text <|
                            " ("
                                ++ String.fromInt nc
                                ++ (if nc > 1 then
                                        " nuits"

                                    else
                                        " nuit"
                                   )
                                ++ ")"
                        )
                    ]
                , row
                    [ width shrink, spacing 5 ]
                    [ text "L’arrivée aura lieu entre :"
                    , el [ Font.weight Font.bold ] (text "16h00 et 20h00")
                    ]
                , row
                    [ width shrink, spacing 5 ]
                    [ text "Le départ aura lieu entre :"
                    , el [ Font.weight Font.bold ] (text "9h00 et 12h00")
                    ]
                , row
                    [ width shrink, spacing 5 ]
                    [ text "Nombre d’adultes :"
                    , el [ Font.weight Font.bold ] (text <| String.fromInt nbrAdults)
                    ]
                , case nbrKids of
                    Just nk ->
                        row
                            [ width shrink, spacing 5 ]
                            [ text "Nombre d’enfants :"
                            , el [ Font.weight Font.bold ] (text <| String.fromInt nk)
                            ]

                    Nothing ->
                        none
                , case pets of
                    Just p ->
                        row
                            [ width shrink, spacing 5 ]
                            [ text "Animaux de compagnie :"
                            , el [ Font.weight Font.bold ] (text p)
                            ]

                    Nothing ->
                        none
                , row
                    [ width shrink, spacing 5 ]
                    [ text "Adresse de la location :"
                    , el [ Font.weight Font.bold ] (text "Gîte Le Vieux Lilas, 5, place de l’Eglise, 89 520 LAINSECQ")
                    ]
                ]

        Nothing ->
            none


montant config model =
    case model.pickedBooking of
        Just { checkIn, checkOut, nbrKids, nbrAdults } ->
            let
                nc =
                    nightsCount checkIn checkOut

                p1 =
                    config.options.basePrice
                        |> Maybe.withDefault 0

                p2 =
                    config.options.multiplier
                        |> Maybe.withDefault 0

                basePrice =
                    p1 + toFloat nc * p2

                opts =
                    config.options.options
                        |> Dict.values
                        |> List.filter .picked
                        |> List.foldr
                            (\o acc -> o.price + acc)
                            0

                tax =
                    0.99 * toFloat nbrAdults * toFloat nc
            in
            column
                [ spacing 20
                , paddingXY 0 30
                ]
                [ column
                    [ spacing 15 ]
                    [ row
                        [ width shrink, spacing 5 ]
                        [ text "Montant total du loyer dû :"
                        , el [ Font.weight Font.bold ]
                            (text <|
                                (euroView <| basePrice + opts + tax)
                                    ++ " €"
                            )
                        ]
                    , row
                        [ width shrink, spacing 5 ]
                        [ text "Dont montant de la taxe de séjour :"
                        , el [ Font.weight Font.bold ]
                            (text <|
                                euroView tax
                                    ++ " €"
                            )
                        ]
                    , row
                        [ width shrink, spacing 5 ]
                        [ text "Montant des arrhes correspondant à 30% du loyer qui sont à verser à la réservation :"
                        , el [ Font.weight Font.bold ]
                            (text <|
                                (euroView <| 0.3 * (basePrice + opts + tax))
                                    ++ " €"
                            )
                        ]
                    ]
                , column
                    [ spacing 15 ]
                    [ Prose.paragraph
                        [ Font.weight Font.bold, Font.lineHeight 1.5 ]
                        [ text "Joindre un chèque (ou la preuve du virement) de ce montant à l’exemplaire de ce contrat à retourner\nau bailleur par courrier ou mail." ]
                    , Prose.paragraph
                        [ Font.weight Font.bold ]
                        [ text "Une caution de 50 € sera demandée à votre arrivée." ]
                    , Prose.paragraph
                        [ Font.lineHeight 1.5 ]
                        [ text "Cette caution vous sera restituée à la fin du séjour sauf en cas de dégradation ou casse." ]
                    , Prose.paragraph
                        [ Font.lineHeight 1.5 ]
                        [ text "Le solde du loyer sera versé au bailleur avant le départ du gîte." ]
                    ]
                ]

        Nothing ->
            none


euroView a =
    let
        f =
            roundC_ a
    in
    if String.contains "." (String.fromFloat f) then
        let
            ( p, s ) =
                ( String.Extra.leftOf "." (String.fromFloat f)
                , String.Extra.rightOf "." (String.fromFloat f)
                )
        in
        p ++ "," ++ String.padRight 2 '0' s

    else
        String.fromFloat f


signaturesDate config =
    column
        [ spacing 30
        , paddingXY 0 30
        ]
        [ row [ width shrink ]
            [ text "Fait en deux exemplaires à "
            , el [ Font.weight Font.bold ] (text "Lainsecq")
            , text ", le "
            , el [ Font.weight Font.bold ]
                (text <|
                    formatDate config.lang
                        (Date.fromPosix config.zone
                            (millisToPosix config.currentTime)
                        )
                )
            ]
        , row
            []
            [ signatureBailleur
            , signaturePreneur
            ]
        ]


signatureBailleur =
    column
        [ spacing 15 ]
        [ text "Le Bailleur"
        , el [] (text "Lu et approuvé")
        , column [ move (left 5) ]
            [ image [ width (px 150) ]
                { source = luEtApprouve
                , description = "lu et approuvé manuscrit"
                }
            , --clipped []
              image [ width (px 150) ]
                { source = signature
                , description = "signature du bailleur"
                }
            ]
        ]


signaturePreneur =
    column
        [ spacing 15 ]
        [ text "Le locataire"
        , el [] (text "Lu et approuvé")
        ]



-------------------------------------------------------------------------------


condGen config =
    let
        parStyle =
            [ Font.lineHeight 1.5
            , Font.justify
            , move (up 5)
            ]
    in
    column
        [ padding 50
        , width (px 1240)
        , height (px 1754)

        --, border 1
        , Font.size 20
        , htmlAttribute <| HA.class "contratPDF"
        , spacing 60
        ]
        [ condGenTitle
        , Prose.paragraph [] [ text "La présente location est faite aux conditions ordinaires et de droit en pareille matière :" ]
        , Prose.numbered
            --Prose.decimal
            [ spacing 40, paddingLeft 20 ]
            [ Prose.item [] <|
                column [ spacing 15 ]
                    [ Prose.paragraph [] [ text "Il est convenu en cas de désistement :" ]
                    , Prose.bulleted
                        --(Prose.custom "➢")
                        [ spacing 15 ]
                        [ Prose.item [] <| Prose.paragraph parStyle [ text "du locataire : à moins d’un mois avant la prise d’effet de la location, le locataire perdra les arrhes versées" ]
                        , Prose.item [] <| Prose.paragraph parStyle [ text "du propriétaire : dans les sept jours suivant le désistement, il est tenu de verser au locataire le double des arrhes reçues." ]
                        ]
                    ]
            , Prose.item [] <| Prose.paragraph parStyle [ text "Obligation d’occuper les lieux personnellement, d’en prendre soin et de les entretenir le temps du séjour. Toutes les installations sont en état de marche et toute réclamation les concernant survenant plus de 24 h après l’entrée en jouissance des lieux, ne pourra être admise. Les réparations rendues nécessaires par la négligence ou le mauvais entretien en cours de location, seront à la charge du locataire." ]
            , Prose.item [] <| Prose.paragraph parStyle [ text "Les locaux sont loués meublés avec matériel de cuisine, vaisselle, verrerie, couettes et oreillers, tels qu’ils sont dans l’état descriptif ci-joint. S’il y a lieu, le propriétaire ou son représentant seront en droit de réclamer au locataire, à son départ, la valeur totale au prix de remplacement des objets, mobiliers ou matériels cassés, fêlés, ébréchés ou détériorés et ceux dont l’usure dépasserait la normale pour la durée de la location, une indemnité pour les détériorations de toute nature concernant les rideaux, papiers peints, plafonds, tapis, vitres, literie, etc. " ]
            , Prose.item [] <| Prose.paragraph parStyle [ text "Le locataire s'engage à s'assurer contre les risques locatifs (incendie, dégât des eaux). Le défaut d'assurance, en cas de sinistre, donnera lieu à des dommages et intérêts." ]
            , Prose.item [] <| Prose.paragraph parStyle [ text "Le propriétaire s'engage à assurer le logement contre les risques locatifs pour le compte du locataire, ce dernier ayant l'obligation de lui signaler, dans les 24 h, tout sinistre survenu dans le logement, ses dépendances ou accessoires." ]
            , Prose.item [] <| Prose.paragraph parStyle [ text "Obligation de veiller à ce que la tranquillité du voisinage ne soit pas troublée par le fait du locataire, de sa famille ou de son animal de compagnie." ]
            , Prose.item [] <| Prose.paragraph parStyle [ text "Le preneur s’engage à respecter la réglementation en vigueur concernant l’accès à l’internet et notamment l’usage et les téléchargements." ]
            ]
        , signaturesDate config
        ]


condGenTitle =
    column
        [ Font.center
        , Font.weight Font.bold
        , spacing 15
        , Font.size 22
        , padding 10
        , background lightGrey
        ]
        [ text "CONDITIONS GENERALES"
        ]



-------------------------------------------------------------------------------


signature =
    "data:image/jpeg;base64,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bnM6cGhvdG9zaG9wPSJodHRwOi8vbnMuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOnRpZmY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vdGlmZi8xLjAvIj4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjExMTk8L2V4aWY6UGl4ZWxZRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpPZmZzZXRUaW1lPiswMTowMDwvZXhpZjpPZmZzZXRUaW1lPgogICAgICAgICA8ZXhpZjpTY2VuZUNhcHR1cmVUeXBlPjA8L2V4aWY6U2NlbmVDYXB0dXJlVHlwZT4KICAgICAgICAgPGV4aWY6RXhwb3N1cmVCaWFzVmFsdWU+MDwvZXhpZjpFeHBvc3VyZUJpYXNWYWx1ZT4KICAgICAgICAgPGV4aWY6RXhwb3N1cmVUaW1lPjEvMzM8L2V4aWY6RXhwb3N1cmVUaW1lPgogICAgICAgICA8ZXhpZjpGb2NhbExlbkluMzVtbUZpbG0+Mjk8L2V4aWY6Rm9jYWxMZW5JbjM1bW1GaWxtPgogICAgICAgICA8ZXhpZjpJU09TcGVlZFJhdGluZ3M+CiAgICAgICAgICAgIDxyZGY6U2VxPgogICAgICAgICAgICAgICA8cmRmOmxpPjIwMDwvcmRmOmxpPgogICAgICAgICAgICA8L3JkZjpTZXE+CiAgICAgICAgIDwvZXhpZjpJU09TcGVlZFJhdGluZ3M+CiAgICAgICAgIDxleGlmOk9mZnNldFRpbWVEaWdpdGl6ZWQ+KzAxOjAwPC9leGlmOk9mZnNldFRpbWVEaWdpdGl6ZWQ+CiAgICAgICAgIDxleGlmOkV4aWZWZXJzaW9uPjAyMzI8L2V4aWY6RXhpZlZlcnNpb24+CiAgICAgICAgIDxleGlmOkZsYXNoIHJkZjpwYXJzZVR5cGU9IlJlc291cmNlIj4KICAgICAgICAgICAgPGV4aWY6RnVuY3Rpb24+RmFsc2U8L2V4aWY6RnVuY3Rpb24+CiAgICAgICAgICAgIDxleGlmOkZpcmVkPkZhbHNlPC9leGlmOkZpcmVkPgogICAgICAgICAgICA8ZXhpZjpSZXR1cm4+MDwvZXhpZjpSZXR1cm4+CiAgICAgICAgICAgIDxleGlmOk1vZGU+MjwvZXhpZjpNb2RlPgogICAgICAgICAgICA8ZXhpZjpSZWRFeWVNb2RlPkZhbHNlPC9leGlmOlJlZEV5ZU1vZGU+CiAgICAgICAgIDwvZXhpZjpGbGFzaD4KICAgICAgICAgPGV4aWY6RXhwb3N1cmVQcm9ncmFtPjI8L2V4aWY6RXhwb3N1cmVQcm9ncmFtPgogICAgICAgICA8ZXhpZjpCcmlnaHRuZXNzVmFsdWU+MjczNzEvMTE2NzU8L2V4aWY6QnJpZ2h0bmVzc1ZhbHVlPgogICAgICAgICA8ZXhpZjpGb2NhbExlbmd0aD44My8yMDwvZXhpZjpGb2NhbExlbmd0aD4KICAgICAgICAgPGV4aWY6Rk51bWJlcj4xMS81PC9leGlmOkZOdW1iZXI+CiAgICAgICAgIDxleGlmOlN1YnNlY1RpbWVEaWdpdGl6ZWQ+OTQ3PC9leGlmOlN1YnNlY1RpbWVEaWdpdGl6ZWQ+CiAgICAgICAgIDxleGlmOldoaXRlQmFsYW5jZT4wPC9leGlmOldoaXRlQmFsYW5jZT4KICAgICAgICAgPGV4aWY6TWV0ZXJpbmdNb2RlPjM8L2V4aWY6TWV0ZXJpbmdNb2RlPgogICAgICAgICA8ZXhpZjpDb21wb25lbnRzQ29uZmlndXJhdGlvbj4KICAgICAgICAgICAgPHJkZjpTZXE+CiAgICAgICAgICAgICAgIDxyZGY6bGk+MTwvcmRmOmxpPgogICAgICAgICAgICAgICA8cmRmOmxpPjI8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaT4zPC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGk+MDwvcmRmOmxpPgogICAgICAgICAgICA8L3JkZjpTZXE+CiAgICAgICAgIDwvZXhpZjpDb21wb25lbnRzQ29uZmlndXJhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjE5NDY8L2V4aWY6UGl4ZWxYRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpFeHBvc3VyZU1vZGU+MDwvZXhpZjpFeHBvc3VyZU1vZGU+CiAgICAgICAgIDxleGlmOkZsYXNoUGl4VmVyc2lvbj4wMTAwPC9leGlmOkZsYXNoUGl4VmVyc2lvbj4KICAgICAgICAgPGV4aWY6QXBlcnR1cmVWYWx1ZT4xNjk0NjMvNzQ0ODk8L2V4aWY6QXBlcnR1cmVWYWx1ZT4KICAgICAgICAgPGV4aWY6U2NlbmVUeXBlPjE8L2V4aWY6U2NlbmVUeXBlPgogICAgICAgICA8ZXhpZjpTdWJqZWN0QXJlYT4KICAgICAgICAgICAgPHJkZjpTZXE+CiAgICAgICAgICAgICAgIDxyZGY6bGk+MjU0OTwvcmRmOmxpPgogICAgICAgICAgICAgICA8cmRmOmxpPjExOTc8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaT43NTM8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaT43NTY8L3JkZjpsaT4KICAgICAgICAgICAgPC9yZGY6U2VxPgogICAgICAgICA8L2V4aWY6U3ViamVjdEFyZWE+CiAgICAgICAgIDxleGlmOlN1YnNlY1RpbWVPcmlnaW5hbD45NDc8L2V4aWY6U3Vic2VjVGltZU9yaWdpbmFsPgogICAgICAgICA8ZXhpZjpTZW5zaW5nTWV0aG9kPjI8L2V4aWY6U2Vuc2luZ01ldGhvZD4KICAgICAgICAgPGV4aWY6T2Zmc2V0VGltZU9yaWdpbmFsPiswMTowMDwvZXhpZjpPZmZzZXRUaW1lT3JpZ2luYWw+CiAgICAgICAgIDxleGlmOlNodXR0ZXJTcGVlZFZhbHVlPjY5NDIxLzEzNzIxPC9leGlmOlNodXR0ZXJTcGVlZFZhbHVlPgogICAgICAgICA8ZXhpZkVYOkxlbnNNb2RlbD5pUGhvbmUgU0UgKDFzdCBnZW5lcmF0aW9uKSBiYWNrIGNhbWVyYSA0LjE1bW0gZi8yLjI8L2V4aWZFWDpMZW5zTW9kZWw+CiAgICAgICAgIDxleGlmRVg6TGVuc1NwZWNpZmljYXRpb24+CiAgICAgICAgICAgIDxyZGY6U2VxPgogICAgICAgICAgICAgICA8cmRmOmxpPjIxNzU3MzMvNTI0MjczPC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGk+MjE3NTczMy81MjQyNzM8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaT4xMS81PC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGk+MTEvNTwvcmRmOmxpPgogICAgICAgICAgICA8L3JkZjpTZXE+CiAgICAgICAgIDwvZXhpZkVYOkxlbnNTcGVjaWZpY2F0aW9uPgogICAgICAgICA8ZXhpZkVYOlBob3RvZ3JhcGhpY1NlbnNpdGl2aXR5PjIwMDwvZXhpZkVYOlBob3RvZ3JhcGhpY1NlbnNpdGl2aXR5PgogICAgICAgICA8ZXhpZkVYOkxlbnNNYWtlPkFwcGxlPC9leGlmRVg6TGVuc01ha2U+CiAgICAgICAgIDxwaG90b3Nob3A6RGF0ZUNyZWF0ZWQ+MjAyNC0wMy0yOVQwOTo1NjowNDwvcGhvdG9zaG9wOkRhdGVDcmVhdGVkPgogICAgICAgICA8eG1wOkNyZWF0ZURhdGU+MjAyNC0wMy0yOVQwOTo1NjowNDwveG1wOkNyZWF0ZURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+MTUuOC4yPC94bXA6Q3JlYXRvclRvb2w+CiAgICAgICAgIDx4bXA6TW9kaWZ5RGF0ZT4yMDI0LTAzLTI5VDA5OjU2OjA0PC94bXA6TW9kaWZ5RGF0ZT4KICAgICAgICAgPHRpZmY6TWFrZT5BcHBsZTwvdGlmZjpNYWtlPgogICAgICAgICA8dGlmZjpSZXNvbHV0aW9uVW5pdD4yPC90aWZmOlJlc29sdXRpb25Vbml0PgogICAgICAgICA8dGlmZjpYUmVzb2x1dGlvbj43MjwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzI8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOkhvc3RDb21wdXRlcj5pUGhvbmUgU0UgKDFzdCBnZW5lcmF0aW9uKTwvdGlmZjpIb3N0Q29tcHV0ZXI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOk1vZGVsPmlQaG9uZSBTRSAoMXN0IGdlbmVy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"


luEtApprouve =
    "data:image/jpeg;base64,iVBORw0KGgoAAAANSUhEUgAAAUAAAABPCAYAAACasRzLAAABdWlDQ1BrQ0dDb2xvclNwYWNlRGlzcGxheVAzAAAokXWQvUvDUBTFT6tS0DqIDh0cMolD1NIKdnFoKxRFMFQFq1OafgltfCQpUnETVyn4H1jBWXCwiFRwcXAQRAcR3Zw6KbhoeN6XVNoi3sfl/Ticc7lcwBtQGSv2AijplpFMxKS11Lrke4OHnlOqZrKooiwK/v276/PR9d5PiFlNu3YQ2U9cl84ul3aeAlN//V3Vn8maGv3f1EGNGRbgkYmVbYsJ3iUeMWgp4qrgvMvHgtMunzuelWSc+JZY0gpqhrhJLKc79HwHl4plrbWD2N6f1VeXxRzqUcxhEyYYilBRgQQF4X/8044/ji1yV2BQLo8CLMpESRETssTz0KFhEjJxCEHqkLhz634PrfvJbW3vFZhtcM4v2tpCAzidoZPV29p4BBgaAG7qTDVUR+qh9uZywPsJMJgChu8os2HmwiF3e38M6Hvh/GMM8B0CdpXzryPO7RqFn4Er/QcXKWq8UwZBywAAAKhlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAABIAAAAAQAAAEgAAAABAAaQAAAHAAAABDAyMjGRAQAHAAAABAECAwCgAAAHAAAABDAxMDCgAgAEAAAAAQAAAUCgAwAEAAAAAQAAAE+kBgADAAAAAQAAAAAAAAAA7oAwlwAAAAlwSFlzAAALEwAACxMBAJqcGAAABHlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDYuMC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzI8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyPC90aWZmOlhSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MTAzNTwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOkNvbG9yU3BhY2U+MTwvZXhpZjpDb2xvclNwYWNlPgogICAgICAgICA8ZXhpZjpTY2VuZUNhcHR1cmVUeXBlPjA8L2V4aWY6U2NlbmVDYXB0dXJlVHlwZT4KICAgICAgICAgPGV4aWY6RXhpZlZlcnNpb24+MDIyMTwvZXhpZjpFeGlmVmVyc2lvbj4KICAgICAgICAgPGV4aWY6Q29tcG9uZW50c0NvbmZpZ3VyYXRpb24+CiAgICAgICAgICAgIDxyZGY6U2VxPgogICAgICAgICAgICAgICA8cmRmOmxpPjE8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaT4yPC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGk+MzwvcmRmOmxpPgogICAgICAgICAgICAgICA8cmRmOmxpPjA8L3JkZjpsaT4KICAgICAgICAgICAgPC9yZGY6U2VxPgogICAgICAgICA8L2V4aWY6Q29tcG9uZW50c0NvbmZpZ3VyYXRpb24+CiAgICAgICAgIDxleGlmOkZsYXNoUGl4VmVyc2lvbj4wMTAwPC9leGlmOkZsYXNoUGl4VmVyc2lvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjI1NDwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgqKoBv6AAAx5ElEQVR4Ae1dB3xUxdaf7Xd7S7Lp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"
