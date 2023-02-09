module Utils exposing (..)

{-| A collection of utility functions.
-}

import List


removeNothings : List (Maybe a) -> List a
removeNothings =
    List.filterMap identity


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        _ ->
            False
