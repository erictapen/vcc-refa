module Artwalk.View exposing (view)

import Artwalk.Model exposing (artwalkPaintings)
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
import Svg.Attributes as SA exposing (xlinkHref)
import Tuple
import Types
import Utils exposing (isNothing, removeNothings)


artwalk : Dict Int (Result String OmekaS.HMO) -> (Int -> String) -> List ( Int, String ) -> Svg Msg
artwalk hmoCache paintingUrl paintings =
    svg []
        [ case Dict.get (Maybe.withDefault 61 <| Maybe.map Tuple.first <| List.head paintings) hmoCache of
            Nothing ->
                text_ [] [ S.text "loading..." ]

            Just (Err e) ->
                text_ [] [ S.text e ]

            Just (Ok (HMO hmo)) ->
                case hmo.thumbnailUrl of
                    Just url ->
                        image [ xlinkHref url ] []

                    Nothing ->
                        text_ [] [ S.text "No thumbnail" ]
        ]


{-| The artwalk view.
-}
view filters typesCache hmoCache =
    div [ id "artwalk" ]
        [ case
            artwalkPaintings typesCache filters
          of
            Nothing ->
                text "Loading"

            -- The intersection of all reverseP67's doesn't yield any results.
            Just [] ->
                text "Zero results. Seems like your filters were to rigid or you didn't select any filters (which isn't implemented yet)."

            Just paintings ->
                artwalk hmoCache (buildUrlRelationalFromId filters) paintings
        ]
