module Artwalk.Model exposing (artwalkPaintings)

import Dict exposing (Dict(..))
import FilterBar.Model exposing (Filters)
import List exposing (map)
import OmekaS exposing (Type(..))
import Result
import Set
import Types
import Utils exposing (isNothing, removeNothings)


{-| TODO implement the case that there is no filter set
We doen't actually have a model for Artwalk, this is just a function that derives one from the Filters set.
Returns Nothing, if we have one ore more misses in the typecache.
-}
artwalkPaintings : Dict Int (Result String Type) -> Filters -> Maybe (List ( Int, String ))
artwalkPaintings typesCache filters =
    let
        setFilters : List Int
        setFilters =
            removeNothings <|
                map (FilterBar.Model.getFilter filters) Types.allFilterTypes

        typeCacheResults : List (Maybe Type)
        typeCacheResults =
            map (\t -> Maybe.andThen Result.toMaybe <| Dict.get t typesCache) setFilters

        typeCacheMiss : Bool
        typeCacheMiss =
            List.any isNothing typeCacheResults
    in
    case typeCacheMiss of
        True ->
            Nothing

        False ->
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
