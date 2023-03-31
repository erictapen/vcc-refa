module Artwalk.View exposing (view)

import Artwalk.Model exposing (artwalkPaintings)
import Browser exposing (UrlRequest(..))
import Constants exposing (baseUrlPath)
import Dict exposing (Dict)
import FilterBar.Model
import Html exposing (Html, a, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import List exposing (map)
import Maybe
import Model exposing (buildUrlRelationalFromId)
import Msg exposing (Msg)
import OmekaS exposing (HMO(..), Type(..))
import String exposing (fromFloat)
import Tuple


painting : Float -> (Int -> String) -> Int -> Maybe (Result String HMO) -> Html Msg
painting globalAnimationFrame paintingUrl index maybeHmo =
    let
        msPerPainting =
            8000

        -- The time, an individual painting is going to spend on the runway. If
        -- it is less than zero, it reached the front and vanishes.
        paintingTime =
            (msPerPainting * toFloat (index + 1)) - globalAnimationFrame

        normalisedPaintingPosition =
            paintingTime / msPerPainting
    in
    -- We only render if the painting hasn't reached the front of the runway yet and if it isn't too far off.
    if paintingTime < 0 || normalisedPaintingPosition > 10 then
        text ""

    else
        case maybeHmo of
            Nothing ->
                div [] [ text "loading..." ]

            Just (Err e) ->
                div [] [ text e ]

            Just (Ok (HMO hmo)) ->
                case hmo.thumbnailUrl of
                    Just url ->
                        a
                            [ href <| paintingUrl hmo.id
                            , class "artwalk-painting"

                            -- enable us to place the picture on its center
                            , style "transform" "translate(-50%, -50%)"

                            , style "position" "absolute"
                            , style "top" "200px"
                            , style "left" ((fromFloat <| 40 * normalisedPaintingPosition) ++ "%")
                            ]
                            [ img [ src url ] []
                            ]

                    Nothing ->
                        div [] [ text "No thumbnail" ]


artwalk :
    Dict Int (Result String OmekaS.HMO)
    -> (Int -> String)
    -> List ( Int, String )
    -> Float
    -> Html Msg
artwalk hmoCache paintingUrl paintings position =
    div
        [ id "artwalk-canvas"
        ]
        [ img
            [ href <| baseUrlPath ++ "/assets/background.png"
            ]
            []
        , div [] <|
            List.reverse <|
                List.indexedMap (painting position paintingUrl) <|
                    map (\i -> Dict.get i hmoCache) <|
                        map Tuple.first <|
                            paintings
        ]


{-| The artwalk view.
-}
view filters typesCache hmoCache position =
    case
        artwalkPaintings typesCache filters
    of
        Nothing ->
            text "Loading"

        -- The intersection of all reverseP67's doesn't yield any results.
        Just [] ->
            text "Zero results. Seems like your filters were to rigid or you didn't select any filters (which isn't implemented yet)."

        Just paintings ->
            artwalk hmoCache (buildUrlRelationalFromId filters) paintings position
