module Utils exposing (httpErrorToString, isNothing, removeNothings)

{-| A collection of utility functions.
-}

import Http exposing (Error(..))
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


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadBody str ->
            "BadBody: " ++ str

        BadUrl str ->
            "BadUrl: " ++ str

        _ ->
            "Unknown HTTP error"
