module Artwalk.View exposing (view)

import Dict
import FilterBar.Model
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import List exposing (map)
import Model exposing (buildUrlRelationalFromId)
import Msg exposing (Msg)
import OmekaS exposing (Type(..))
import Set
import Types
import Utils exposing (isNothing, removeNothings)


paintingItem : (Int -> String) -> ( Int, String ) -> Html Msg
paintingItem paintingUrl ( id, name ) =
    li [] [ a [ href <| paintingUrl id ] [ text name ] ]


{-| The artwalk view.
-}
view filters typesCache hmoCache =
    div []
        [ h1 [] [ text "Artwalk view" ]
        , let
            setFilters : List Int
            setFilters =
                removeNothings <|
                    map (FilterBar.Model.getFilter filters) Types.allFilterTypes

            typeCacheResults : List (Maybe Type)
            typeCacheResults =
                map (\t -> Dict.get t typesCache) setFilters

            typeCacheMiss : Bool
            typeCacheMiss =
                not <| List.all identity <| map (not << isNothing) typeCacheResults
          in
          if typeCacheMiss then
            -- We don't have every of the four possible filters in the type cache yet.
            text "Loading"

          else
            case
                Maybe.map Set.toList <|
                    List.foldr
                        (\(Type tr) maybeSet ->
                            case maybeSet of
                                Nothing ->
                                    Just <| Set.fromList tr.reverseP67

                                Just set ->
                                    Just <| Set.intersect set <| Set.fromList tr.reverseP67
                        )
                        Nothing
                    <|
                        removeNothings typeCacheResults
            of
                Nothing ->
                    text "Artwalk for no filters at all is not implemented yet. Please make a choice."

                -- The intersection of all reverseP67's doesn't yield any results.
                Just [] ->
                    text "Zero results. Seems like your filters were to rigid. Try removing some!"

                Just paintings ->
                    ul [] <| map (paintingItem (buildUrlRelationalFromId filters)) paintings
        ]
