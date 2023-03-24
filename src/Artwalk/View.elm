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
import String exposing (fromFloat)
import Svg as S exposing (Svg, image, svg, text_)
import Svg.Attributes as SA exposing (viewBox, xlinkHref)
import Svg.Events as SA exposing (onClick)
import Tuple


painting : Float -> (Int -> String) -> Int -> Maybe (Result String HMO) -> Svg Msg
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
        S.text ""

    else
        case maybeHmo of
            Nothing ->
                text_ [ SA.x "50", SA.y "50" ] [ S.text "loading..." ]

            Just (Err e) ->
                text_ [ SA.x "50", SA.y "50" ] [ S.text e ]

            Just (Ok (HMO hmo)) ->
                case hmo.thumbnailUrl of
                    Just url ->
                        S.a
                            [ xlinkHref <| paintingUrl hmo.id
                            ]
                            [ image
                                [ xlinkHref url
                                , SA.width "20"

                                -- , SA.height "20"
                                , SA.x "0"
                                , SA.y "0"
                                , SA.transform <|
                                    String.join " "
                                        [ "scale("
                                            ++ (fromFloat <| 1 / (normalisedPaintingPosition + 1))
                                            ++ ") "
                                        , "translate(50 10)"
                                        , "translate("
                                            ++ (fromFloat <| 40 * normalisedPaintingPosition)
                                            ++ " 0) "
                                        ]
                                ]
                                []
                            ]

                    Nothing ->
                        text_ [] [ S.text "No thumbnail" ]


artwalk :
    Dict Int (Result String OmekaS.HMO)
    -> (Int -> String)
    -> List ( Int, String )
    -> Float
    -> Svg Msg
artwalk hmoCache paintingUrl paintings position =
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
        , S.g [] <|
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
