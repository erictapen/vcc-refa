module Relational.View exposing (view)

import Constants exposing (refaBaseUrl)
import Dict exposing (Dict)
import FilterBar.Model
import Html exposing (Html, a, details, div, h1, h2, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (href, src, style)
import List exposing (map)
import Model exposing (ArtwalkMode(..), buildUrl, buildUrlRelationalFromId)
import Msg exposing (Msg)
import OmekaS exposing (HMO(..), Type(..))
import String exposing (fromInt)
import Types


{-| One of the four widgets that display pictures belonging to the selected category
-}
relationalTile :
    (Int -> String)
    -> Maybe Type
    -> Maybe Int
    -> Dict Int (Result String HMO)
    -> Types.FilterType
    -> Html Msg
relationalTile paintingUrl maybeType maybeTypeId hmoCache filterType =
    div []
        [ h2 [] [ text <| Types.toString filterType ]
        , case ( maybeTypeId, maybeType ) of
            ( Nothing, _ ) ->
                text <| "Select a filter for " ++ Types.toString filterType ++ "."

            ( _, Nothing ) ->
                text "Loading.."

            ( Just _, Just (Type tr) ) ->
                ul [] <| map (paintingItem paintingUrl) tr.reverseP67
        ]


{-| The relational view
-}
view typesCache hmoCache paintingId filters =
    div []
        [ a
            [ style "float" "right"
            , href <| buildUrl (Artwalk { position = 0 }) filters
            ]
            [ text "Back to Artwalk" ]
        , h1 [] [ text "Relational view" ]
        , div []
            [ p []
                [ a [ href <| refaBaseUrl ++ fromInt paintingId ]
                    [ text "Link to the ReFa web interface" ]
                ]
            , case Dict.get paintingId hmoCache of
                Nothing ->
                    text "Loading..."

                Just (Err err) ->
                    text err

                Just (Ok (HMO hmoData)) ->
                    case hmoData.thumbnailUrl of
                        Nothing ->
                            text "Kein Thumbnail!"

                        Just thumbnailUrl ->
                            div []
                                [ img [ src thumbnailUrl ] []
                                , div [] <|
                                    map
                                        (\ft ->
                                            relationalTile
                                                (buildUrlRelationalFromId filters)
                                                (Maybe.andThen
                                                    (\t -> Dict.get t typesCache)
                                                    (FilterBar.Model.getFilter filters ft)
                                                )
                                                (FilterBar.Model.getFilter filters ft)
                                                hmoCache
                                                ft
                                        )
                                        Types.allFilterTypes
                                , div []
                                    [ h2 []
                                        [ text "Debug view"
                                        ]
                                    , text "This view is for debugging purposes only and will eventually be removed"
                                    , ul [] <|
                                        map
                                            (tagListItem
                                                (buildUrlRelationalFromId filters)
                                                typesCache
                                            )
                                            hmoData.p67refersTo
                                    ]
                                ]
            ]
        ]


{-| This list of tags is currently only shown for developing purposes.
Eventually we are going to show only one tag.
-}
tagListItem : (Int -> String) -> Dict Int Type -> Int -> Html Msg
tagListItem paintingUrl typesCache typesId =
    li [] <|
        case Dict.get typesId typesCache of
            Nothing ->
                [ text "Loading..." ]

            Just (Type t) ->
                [ details []
                    [ let
                        refaUrl : String
                        refaUrl =
                            refaBaseUrl ++ fromInt typesId
                      in
                      summary []
                        (Maybe.withDefault
                            [ text <| fromInt typesId ++ ": "
                            , a [ href <| refaUrl ] [ text t.label ]
                            ]
                         <|
                            Maybe.map
                                (\( fType, label ) ->
                                    [ span [ style "font-weight" "bold" ]
                                        [ text <| Types.toString fType ++ ": " ]
                                    , a [ href <| refaUrl ] [ text label ]
                                    ]
                                )
                                (Dict.get typesId Types.filterTypeRegistry)
                        )
                    , span [] [ ul [] <| map (paintingItem paintingUrl) t.reverseP67 ]
                    ]
                ]


paintingItem : (Int -> String) -> ( Int, String ) -> Html Msg
paintingItem paintingUrl ( id, name ) =
    li [] [ a [ href <| paintingUrl id ] [ text name ] ]
