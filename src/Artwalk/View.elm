module Artwalk.View exposing (view)

import Artwalk.Model exposing (artwalkPaintings)
import Browser exposing (UrlRequest(..))
import Constants exposing (baseUrlPath)
import Dict exposing (Dict)
import FilterBar.Model
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href, id)
import List exposing (map)
import Maybe
import Model exposing (buildUrlRelationalFromId)
import Msg exposing (Msg)
import OmekaS exposing (HMO(..), Type(..))
import Result
import Set
import Svg as S exposing (Svg, image, svg, text_)
import Svg.Attributes as SA exposing (viewBox, xlinkHref)
import Svg.Events as SA exposing (onClick)
import Tuple
import Types
import Url
import Utils exposing (isNothing, removeNothings)


artwalk : Dict Int (Result String OmekaS.HMO) -> (Int -> String) -> List ( Int, String ) -> Svg Msg
artwalk hmoCache paintingUrl paintings =
    svg
        [ id "artwalk-svg"
        , viewBox "0 0 100 100"
        ]
        [ image
            [ xlinkHref <| baseUrlPath ++ "/assets/background.png"
            , SA.y "-15%"
            , SA.width "100%"
            ]
            []
        , case Dict.get (Maybe.withDefault 61 <| Maybe.map Tuple.first <| List.head paintings) hmoCache of
            Nothing ->
                text_ [ SA.x "50", SA.y "50" ] [ S.text "loading..." ]

            Just (Err e) ->
                text_ [ SA.x "50", SA.y "50" ] [ S.text e ]

            Just (Ok (HMO hmo)) ->
                case hmo.thumbnailUrl of
                    Just url ->
                        S.a [ xlinkHref <| paintingUrl hmo.id
                            ]
                            [ image
                                [ xlinkHref url
                                , SA.width "20"
                                -- , SA.height "20"
                                , SA.x "50%"
                                , SA.y "10%"
                                , SA.transform "translate(-10 0)"
                                ]
                                []
                            ]

                    Nothing ->
                        text_ [] [ S.text "No thumbnail" ]
        ]


{-| The artwalk view.
-}
view filters typesCache hmoCache =
    case
        artwalkPaintings typesCache filters
    of
        Nothing ->
            text "Loading"

        -- The intersection of all reverseP67's doesn't yield any results.
        Just [] ->
            text "Zero results. Seems like your filters were to rigid or you didn't select any filters (which isn't implemented yet)."

        Just paintings ->
            artwalk hmoCache (buildUrlRelationalFromId filters) paintings
