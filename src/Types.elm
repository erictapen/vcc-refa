module Types exposing (filterTypes, toString)

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


filterTypes : Dict Int ( FilterType, String )
filterTypes =
    Dict.fromList
        [ ( 10217, ( Accessories, "Necklace" ) )
        , ( 10300, ( Accessories, "Flowers" ) )
        , ( 10266, ( Accessories, "Fan" ) )
        , ( 10293, ( Head, "Wig (Woman)" ) )
        , ( 10200, ( UpperBody, "Dress" ) )
        ]
