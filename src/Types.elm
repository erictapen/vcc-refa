module Types exposing (FilterType(..), filterTypes, toIdentifier, toString)

import Dict exposing (Dict)


type FilterType
    = Head
    | UpperBody
    | LowerBody
    | Accessories


toString : FilterType -> String
toString ft =
    case ft of
        Head ->
            "Head"

        UpperBody ->
            "Upper body"

        LowerBody ->
            "Lower body"

        Accessories ->
            "Accessories"


{-| This has to produce unique strings!
-}
toIdentifier : FilterType -> String
toIdentifier ft =
    case ft of
        Head ->
            "FilterHead"

        UpperBody ->
            "FilterUpperbody"

        LowerBody ->
            "FilterLowerbody"

        Accessories ->
            "FilterAccessories"


filterTypes : Dict Int ( FilterType, String )
filterTypes =
    Dict.fromList
        [ ( 10217, ( Accessories, "Necklace" ) )
        , ( 10300, ( Accessories, "Flowers" ) )
        , ( 10266, ( Accessories, "Fan" ) )
        , ( 10293, ( Head, "Wig (Woman)" ) )
        , ( 10200, ( UpperBody, "Dress" ) )
        ]
