module FilterBar.Model exposing (..)

import Select
import Types


type alias SelectElement =
    { selectState : Select.State
    , selectedItem : Maybe Int
    }


emptySelect filterType =
    { selectState = Select.initState (Select.selectIdentifier <| Types.toIdentifier filterType)
    , selectedItem = Nothing
    }


type alias Filters =
    { head : Maybe Int
    , upperBody : Maybe Int
    , lowerBody : Maybe Int
    , accessories : Maybe Int
    }


emptyFilters =
    { head = Nothing
    , upperBody = Nothing
    , lowerBody = Nothing
    , accessories = Nothing
    }


{-| A utility function to quickly get the correct filter from Filters.
-}
getFilter : Filters -> Types.FilterType -> Maybe Int
getFilter filters filterType =
    case filterType of
        Types.Head ->
            filters.head

        Types.UpperBody ->
            filters.upperBody

        Types.LowerBody ->
            filters.lowerBody

        Types.Accessories ->
            filters.accessories
